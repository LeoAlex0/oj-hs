# Haskell Bundler 设计文档

## Context

本 change 要新增一个 `haskell-bundler` CLI，把选定 package executable 及其可达内部模块打包成单个可独立编译的 `.hs` 文件。当前仓库已有多个 executable，其中 `luogu-wip` 和 `codeforces-wip` 是较薄的题解入口，适合作为 MVP 验证目标；`all-in-one` 包含 Template Haskell 和旧的 source-level 打包思路，适合作为后续压力测试参考。

核心约束是：不要重新实现 GHC 已经提供的语义分析能力。Bundler 应通过 Cabal 配置 GHC 会话，让 GHC 完成模块发现、import 解析、依赖排序、循环检测、CPP 展开、Template Haskell 执行、renaming、typechecking 和 Core 简化。Bundler 自身只负责把 GHC 的结果映射成单文件输出：内部符号重命名、外部引用 qualified 化、内部模块展平、构造候选 `Main` 分析模块、基于该候选模块的 Core live set 裁剪源码声明，最后写入输出。

```
┌──────────────────┐
│ CLI options      │
└────────┬─────────┘
         ▼
┌──────────────────┐
│ Cabal metadata   │
│ executable stanza│
└────────┬─────────┘
         ▼
┌────────────────────────────────────┐
│ GHC session + load                  │
│ - ModuleGraph                       │
│ - RenamedSource                     │
│ - TypecheckedSource                 │
│ - CPP / TH diagnostics              │
└────────┬───────────────────────────┘
         ▼
┌────────────────────────────────────┐
│ Bundler transforms                  │
│ - internal Name -> generated ident  │
│ - external Name -> Full.Module.ident│
│ - generated ident -> decl group map │
│ - candidate Main (main) source      │
└────────┬───────────────────────────┘
         ▼
┌────────────────────────────────────┐
│ GHC Core simplifier on candidate    │
│ - in-memory GHC targetContents      │
│ - simplified Core bindings          │
│ - live internal Names / Ids          │
└────────┬───────────────────────────┘
         ▼
┌────────────────────────────────────┐
│ Final source rendering              │
│ - Core live set -> source pruning    │
└────────┬───────────────────────────┘
         ▼
┌──────────────────┐
│ bundled.hs       │
└──────────────────┘
```

## Goals / Non-Goals

**Goals:**

- 支持通过 `--exec <name>` 选择 package executable，未指定时选择第一个 executable。
- 支持通过 `--output <path>` 指定输出路径，未指定时输出 `bundled.hs`。
- 使用 Cabal 元数据配置 GHC 9.6.7 会话，而不是手写 source dirs、依赖或 package db 推断。
- 使用 GHC `ModuleGraph` 和 GHC 解析后的 `Name` 信息作为模块和符号事实来源。
- 将可达内部模块展平为单个显式只导出 `main` 的 `Main` 模块，并生成可独立编译的源码。
- 对内部符号进行确定性重命名，避免展平后的顶层命名冲突，并保持原 occurrence 的 Haskell 词法类别。
- 对普通外部包引用使用完整模块路径 qualified 形式，统一生成无 alias 的 qualified import；当编译环境中存在同名外部模块时，使用 package-qualified import 消歧。
- 从 MVP 开始对展平后的候选 `Main` 分析模块接入 GHC Core 简化管线，使用 Core live bindings 完成可达性分析和源码声明裁剪。

**Non-Goals:**

- MVP 不实现手写依赖图、手写 import 解析、手写循环检测或手写 TH/CPP 展开。
- MVP 不实现自定义 source-level 可达性 BFS；可达性由 GHC Core 简化结果提供。
- MVP 不追求保留原始源码格式、注释或 layout；输出优先保证可编译。
- MVP 不要求完整 self-bootstrap；自举测试是后续阶段验证目标。
- MVP 不将 Core 反编译成 Haskell 源码；Core 只作为 live set 和裁剪依据。
- MVP 不承诺支持所有复杂 Haskell 扩展组合；遇到 GHC AST/ppr 难点时以明确错误或保守保留为准。

## Decisions

### DD-1: 以 `package.yaml` 为源，Cabal 文件为生成物

**决策**：实现和任务以更新 `package.yaml` 为准，再重新生成 `oj-hs.cabal`。

**理由**：仓库当前 `oj-hs.cabal` 由 hpack 生成，直接长期维护 cabal 文件会产生漂移。

**替代方案**：只改 `oj-hs.cabal`。该方案短期可行，但会在下一次 hpack 生成时丢失配置。

### DD-2: 通过 Cabal 配置 GHC session

**决策**：读取 Cabal package description 和 selected executable stanza，用其中的 source dirs、依赖、extensions、compiler options 初始化 GHC。

**理由**：Cabal 已经掌握包数据库、依赖、扩展和编译选项。Bundler 复刻这些规则会产生大量边界错误。

**替代方案**：手写 source dirs 遍历和 package 解析。该方案与设计目标冲突，并且难以正确处理 conditional、default extensions 和 package db。

### DD-3: 用 GHC load 和 ModuleGraph 代替自定义依赖分析

**决策**：将选定 executable module 设置为 GHC target，调用 `load`，之后从 GHC 的 module graph 和 summaries 获取可达内部模块及顺序。

**理由**：GHC 编译管理器已经完成模块发现、import 解析、拓扑排序和循环检测。Bundler 只读取结果。

**替代方案**：实现 `Bundler.Dependencies` 递归遍历。该方案已从任务列表移除，因为它会重复 GHC 能力，并且容易与真实编译行为不一致。

### DD-4: 用 GHC `Name` 身份驱动符号变换

**决策**：内部/外部符号分类和重命名都基于 GHC renamer/typechecker 产生的 `Name`，而不是 `rdrName` 或文本 occurrence 猜测。

**理由**：`Name` 携带定义模块和唯一身份，能区分相同 occurrence name 的不同绑定。展平后冲突处理必须依赖这个级别的信息。

**替代方案**：基于文本名字和 import 列表推断。该方案无法可靠处理 re-export、qualified/unqualified 混合导入、同名类型/构造器/字段和不同模块同名符号。

### DD-5: 内部符号确定性重命名并保持词法类别

**决策**：内部定义生成稳定的新名称，形式以模块路径派生前缀加 occurrence name 为主；若生成冲突，报告冲突或使用确定性消歧后缀。生成名称不能只是文本 sanitize，必须保持原 occurrence 在 Haskell lexer 中的类别：`varid` 仍生成 `varid`，`conid` 仍生成 `conid`，`varsym` 仍生成 `varsym`，`consym` 仍生成 `consym`。

**理由**：单文件输出中所有内部模块共享一个顶层命名空间，必须消除模块边界带来的同名风险。确定性命名也是 bootstrap 测试的基础。

**实现边界**：名称生成需要基于 GHC `OccName` 的 namespace 和 spelling 选择合法输出形式，而不是把所有非法 identifier 字符统一转成字母数字。特别是 operator 名称的定义、引用和 fixity declarations 必须同步处理：MVP 要求生成合法的 operator spelling 并保持 operator 词法类别；即使某些使用点被渲染为 prefix form，也必须使用带括号的 operator 前缀写法，不能改成普通 identifier。fixity declarations 要么同步重写到生成后的 operator 名称，要么只在确认最终输出不需要 infix 解析时省略。

| 原 occurrence 类别 | 生成名称要求 |
| --- | --- |
| `varid` | 生成合法小写 identifier |
| `conid` | 生成合法大写 identifier |
| `varsym` | 生成合法变量 operator，不能改成 `varid` |
| `consym` | 生成合法构造器 operator，不能改成 `conid` |

**替代方案**：尽量保留原名，只在检测到冲突时改名。该方案输出更接近原始源码，但实现复杂，并且内部引用重写仍然不可避免。

### DD-6: 外部引用统一为完整模块路径 qualified

**决策**：普通外部模块 import 输出为 `import qualified <Full.Module.Name>`，引用渲染为 `<Full.Module.Name>.<symbol>`，不生成 alias。当 GHC `Name` 的定义模块来自外部包，且最终源码可能在暴露多个同名模块的环境中编译时，import 应带 package qualifier，例如 `import qualified "ghc" GHC.Core`，并为输出启用 `PackageImports`。

**理由**：完整模块路径天然区分 `Data.Map`、`Data.Map.Strict`、`Data.Vector` 等模块，避免 alias 分配和冲突管理。

**实现边界**：最终输出不依赖保留原始 import alias。GHC pretty-printer 在某些 AST phase 下可能丢失或改变原始 external qualifier，这不是核心语义问题；Bundler 输出层需要从外部 `Name` 的定义模块重新渲染 `<Full.Module.Name>.<symbol>`，并统一生成无 alias 的 qualified imports。

**边界**：Prelude、语法脱糖相关名字和启用 `NoImplicitPrelude` / `RebindableSyntax` 后的隐式名字不能简单按普通外部引用处理。MVP 应优先覆盖 `luogu-wip` 和 `codeforces-wip` 的普通 Prelude 行为；`all-in-one` 的 `NoImplicitPrelude` 场景需要单独 fixture 验证，必要时在该阶段给出明确限制。

**替代方案**：保留原始 import 样式或生成短 alias。保留原样会在展平后制造非限定冲突；短 alias 需要额外的确定性分配和冲突处理。

### DD-7: 输出优先使用 GHC pretty-printer，但允许阶段化 fallback

**决策**：输出使用 GHC `Outputable` / `SDoc` 体系作为基础。普通模块可以优先使用能稳定 pretty-print 的 `GhcRn`/`GhcTc` 声明；包含 Template Haskell 展开结果的模块不得静默回退到未展开的 `GhcRn` splice。如果 TH-expanded declaration 无法转换为可输出源码，系统应报告明确限制，而不是生成仍含未展开 splice 的最终输出。直接 ppr 改写后的 `RenamedSource` 不足以作为完整输出契约，除非输出层已经显式控制内部名称 spelling、外部 qualifier 和 operator fixity declarations。

**理由**：GHC ppr 能输出语法正确的 Haskell 片段，但 typed AST 中可能存在不适合直接作为源代码输出的构造。设计需要承认这个风险，同时保持 proposal/spec 中“TH 由 GHC 展开”的语义承诺。

**替代方案**：引入 `ghc-exactprint` 保留原始格式。该方案更重，且 TH 展开后仍不等同于原始源码保留，暂不作为 MVP 依赖。

### DD-8: TH/CPP 由 GHC 执行，Bundler 只接收结果和错误

**决策**：CPP 和 Template Haskell 不做自定义处理。CPP 在 GHC parse 前完成；TH 在 GHC typecheck 时执行。Bundler 记录 GHC 输出，并透传失败诊断。若 TH 展开结果本身包含构建环境信息，该信息属于用户代码在当前 GHC 会话中的语义结果，Bundler 不额外清洗或替换。

**理由**：TH/CPP 语义依赖编译环境、flags 和包依赖，只有 GHC session 能正确处理。

**替代方案**：保留 TH splice 原样或手写展开逻辑。保留原样不满足“输出包含展开后代码”的目标；手写展开不可行。

### DD-9: 从 MVP 开始接入 GHC Core 简化管线

**决策**：`Bundler.DCE` 从一开始运行 GHC Core 生成与简化管线。为避免 per-module Core simplification 把模块 exports 误当成全部必须保留，Bundler 先构造一个展平后的候选 `Main` 分析模块，源码头使用 `module Main (main) where`，只有最终入口 `main` 作为外部可见入口。随后对该候选模块运行 GHC Core simplifier，并从简化后的 Core bindings 中提取 live internal `Name` / `Id` 集合。源码输出仍来自 transformed Haskell declarations；Core live set 只用于决定哪些源码声明或声明组保留。

**理由**：GHC Core 简化管线已经包含内联、字典、worker/wrapper、规则和 dead-code elimination 等语义信息。用 Core 结果做可达性分析，比手写 source-level BFS 更接近真实编译行为，也能避免重复实现 GHC 已经具备的优化逻辑。

**实现边界**：Bundler 不把 Core 反编译成 Haskell。Core pipeline 只产出 live set；源码层负责记录“原始 GHC `Name` → 生成标识符 → 候选模块声明组”的映射，并按声明或声明组粒度裁剪。无法安全映射或删除后会破坏可编译性的声明，应诊断或保留。

**替代方案**：手写 GHC `Name` 驱动的保守 source-level 可达性分析。该方案实现较轻，但会重复 GHC 能力，并且对类型类字典、派生代码、RULES 和优化后可达性都不够可靠，因此不采用。

### DD-10: 候选模块通过 in-memory GHC target 进入 Core 管线

**决策**：候选 `Main` 分析模块先渲染为一段可编译 Haskell source string，再通过 GHC 9.6.7 的 `Target` / `targetContents` 作为内存文件加载、parse、typecheck、desugar 和 simplify。实现不手写 `ModGuts`，也不把源 AST 直接拼成 Core。

**理由**：内存 target 复用 GHC parser、renamer、typechecker、desugarer 和 Core simplifier，同时避免把候选文件落盘。它也让候选模块和最终输出使用同一份 Haskell 源表示，降低 Core live set 与源码裁剪之间的漂移风险。

**实现边界**：候选分析应使用与选定 executable 一致的 package、extension 和 optimization 配置。为避免与原始 executable 的 `Main` module target 冲突，候选分析可以使用独立或重置后的 GHC session，并只加载内存中的 `BundledCandidate.hs` target。

**替代方案**：直接构造 `ModGuts`。该方案绕过 parser/typechecker，要求 Bundler 维护更多 GHC phase invariant，短期风险更高。

### DD-11: pragma 和 Prelude 行为阶段化处理

**决策**：展平候选模块需要合并声明可编译所需的 language pragmas，但不能无条件合并会改变全局名字解析语义的扩展。`NoImplicitPrelude`、`RebindableSyntax`、`QualifiedDo` 等会影响隐式名字解析的扩展需要通过 fixture 验证后再扩大支持范围。

**理由**：原始多模块项目中，扩展和隐式 Prelude 行为可以按模块生效；展平成单个模块后这些开关会变成全局语义。盲目取并集可能让原本依赖隐式 Prelude 的声明无法解析，也可能改变脱糖结果。

**替代方案**：直接取所有模块 pragmas 的并集。该方案实现简单，但对 Prelude 和语法脱糖类扩展不安全。

### DD-12: 分阶段验证

**决策**：MVP 先验证 `luogu-wip` 和 `codeforces-wip` 能生成并独立编译；`all-in-one`、完整 TH/CPP fixture 和 self-bootstrap 放到后续阶段。

**理由**：薄 executable 能验证 Cabal/GHC 初始化、模块加载、展平和基本命名链路。TH 和 bootstrap 是更高复杂度验证，不应阻塞第一条可工作的打包路径。

**替代方案**：先做 self-bootstrap。该方案覆盖面强，但会把所有难点一次性压到 MVP，调试成本过高。

## Implementation Anchors

本地环境已确认使用 GHC 9.6.7。实现时可以把以下 GHC API 作为初始锚点，后续再按真实类型错误微调：

```haskell
setTargets      :: GhcMonad m => [Target] -> m ()
load            :: GhcMonad m => LoadHowMuch -> m SuccessFlag
getModuleGraph  :: GhcMonad m => m ModuleGraph
parseModule     :: GhcMonad m => ModSummary -> m ParsedModule
typecheckModule :: GhcMonad m => ParsedModule -> m TypecheckedModule
desugarModule   :: GhcMonad m => TypecheckedModule -> m DesugaredModule
coreModule      :: DesugaredModule -> ModGuts
hscSimplify     :: HscEnv -> [String] -> ModGuts -> IO ModGuts
```

候选模块的 in-memory target 形状应围绕 GHC 9.6.7 的 `Target` 记录展开：

```haskell
Target
  { targetId = TargetFile "BundledCandidate.hs" Nothing
  , targetAllowObjCode = False
  , targetUnitId = selectedUnitId
  , targetContents = Just (stringToStringBuffer candidateSource, timestamp)
  }
```

候选源码必须从 `module Main (main) where` 开始。`targetContents` 里的 path 和 timestamp 只用于 GHC 诊断、缓存和 source location；最终输出不得包含 bundler 自身为候选模块或临时源文件额外引入的时间戳、临时路径或本地 store path。用户源码、CPP 或 TH 展开明确生成的环境值应按 GHC 结果保留。

## Risks / Trade-offs

- **GHC API 版本绑定** → 项目固定 GHC 9.6.7；升级 GHC 时需要适配 API 和 AST 构造变化。
- **Cabal 与 GHC session 配置不完整** → MVP 需要优先把 selected executable 的 source dirs、extensions、options、package deps 传准；错误信息应暴露 Cabal/GHC 原始诊断。
- **Template Haskell 运行时依赖** → Nix 环境必须能加载 TH 所需依赖；失败时透传 GHC 诊断。
- **`GhcTc` 直接 ppr 风险** → 输出层需要准备回退策略；某些 TH-expanded 构造可能需要单独 spike。
- **Core live set 到源码声明映射风险** → Core 中的 optimized binding 不一定一一对应源码声明；实现需要保留声明组粒度，并对无法映射的声明诊断或保留。
- **per-module Core DCE 误判风险** → 普通模块编译会保留 exports；因此 DCE 输入必须是展平候选 `Main` 分析模块，而不是逐个内部模块独立简化后的 Core。
- **Name 替换后的 AST phase 一致性** → 直接改 AST 可能触碰 GHC phase invariants；实现时应把“可 ppr 的输出表示”与“GHC 原始 AST”边界隔离。
- **operator 重命名语法风险** → `varsym` / `consym` 不能改写成普通 identifier；否则 fixity declaration、infix pattern 和 expression 会变成非法或语义漂移的源码。
- **external qualifier 渲染风险** → 直接依赖 GHC ppr 可能丢失原始 alias 或 qualifier；输出层必须按外部 `Name` 的定义模块重新渲染完整模块限定名。
- **类型类 instances 和 orphan instances** → 展平会改变模块边界；Core live set 能帮助判断实际使用情况，但源码层删除 instance 仍需按可编译性验证约束。
- **Core pipeline 配置风险** → Core 简化必须使用与 selected executable 一致的 DynFlags、optimization flags 和 package environment，否则 live set 可能与真实编译不一致。
- **pragma 全局化风险** → 多模块的 per-module language pragmas 展平后会变成单模块全局设置，尤其是 `NoImplicitPrelude`、`RebindableSyntax` 和语法扩展，需要通过 fixture 控制支持范围。
- **候选 `Main` 与原始 `Main` 冲突风险** → 原始 executable 通常也是 `Main` 模块；候选分析应使用独立或重置后的 GHC session 加载内存 target。
- **输出格式不可读** → GHC ppr 不保留注释和原 layout；可在后续提供 `ormolu`/`fourmolu` 后处理。
- **确定性输出** → 需要对模块、imports、声明和生成名称排序；bootstrap 之前仍需用普通集成测试防止 nondeterminism。

## Migration Plan

1. 先完成项目结构、CLI、Cabal/GHC session 和 module loading。
2. 再完成基于 `Name` 的内部重命名和外部 qualified 引用。
3. 接入 flatten，生成可供分析的候选 `module Main (main) where` 源码表示，并记录生成标识符到声明组的映射。
4. 通过 in-memory GHC target 加载候选 `Main` 模块，接入 GHC Core 简化管线并提取 live internal names。
5. 将 Core live set 映射回 transformed Haskell declarations，并按声明或声明组裁剪。
6. 渲染最终 `bundled.hs`，并对薄 executable 做独立编译验证，确保 Core pruning 后仍可编译。
7. 再处理 TH/CPP fixture、`all-in-one` 压力测试和 self-bootstrap。

回滚策略很简单：新功能是新增 executable 和 `src/Bundler/` 模块，不影响现有 library/executables；如果实现不可用，可从 package 配置中移除 `haskell-bundler` executable。

## Open Questions

- `TypecheckedSource` 中 TH-expanded declaration 到可输出源码的具体边界需要实现 spike 验证。
- GHC 9.6.7 中可用的 module graph/topological order API 具体名称需要在实现时确认。
- 内部名称生成策略是“模块前缀 + occurrence”即可，还是默认加入稳定 hash 后缀，需要通过真实冲突样例决定；该策略必须分别覆盖 `varid`、`conid`、`varsym` 和 `consym`。
- 候选 `Main` in-memory target 的最小实现细节需要 spike 固化，包括 target path、timestamp、session reset 和诊断定位。
- Core simplifier pipeline 需要保留哪些 flags 才能稳定提取适合源码裁剪的 live internal names，需要实现 spike 验证。
- Core live names 到源码声明组的映射粒度需要通过真实模块样例确认。
- `NoImplicitPrelude`、`RebindableSyntax` 和 TH-expanded declaration 的组合是否进入 MVP，需要通过 `all-in-one` 或专门 fixture 决定。
