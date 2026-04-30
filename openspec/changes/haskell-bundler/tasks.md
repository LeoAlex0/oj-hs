# Haskell Bundler - 实施任务

## 1. 项目结构

- [x] 1.1 在 `src/Bundler/` 下创建 `Bundler` 模块结构
- [x] 1.2 在 `package.yaml` 中加入所需的 Cabal、GHC API 和 CLI 依赖
- [x] 1.3 在 `app/bundler/Main.hs` 下新增 `haskell-bundler` 可执行入口
- [x] 1.4 从 `package.yaml` 重新生成 `oj-hs.cabal`
- [x] 1.5 验证新可执行文件能在现有 Nix/Cabal 环境中构建

## 2. CLI 接口

- [x] 2.1 使用 `optparse-applicative` 实现 `Bundler.Options`
- [x] 2.2 解析用于选择可执行文件的 `--exec <name>`
- [x] 2.3 解析 `--output <path>`，默认值为 `bundled.hs`
- [x] 2.4 对无效参数输出标准 CLI usage 信息
- [x] 2.5 对未知可执行文件名输出包内可用可执行文件列表

## 3. Cabal 与 GHC 会话初始化

- [x] 3.1 创建 `Bundler.Env`，保存包元数据、选定可执行文件、GHC 会话和已加载模块数据
- [x] 3.2 通过 Cabal API 读取包元数据和 executable stanza
- [x] 3.3 选择请求的可执行文件；未指定时默认选择第一个 package executable
- [x] 3.4 使用选定可执行文件的 source dirs、依赖、扩展和 compiler options 初始化 GHC 9.6.7
- [x] 3.5 将选定可执行模块设置为 GHC target 并调用 `load`
- [x] 3.6 将 Cabal 与 GHC load 诊断封装为 `BundleError`

## 4. GHC 驱动的模块分析

- [x] 4.1 从 GHC 读取已加载的 `ModuleGraph`，不构建自定义依赖图
- [x] 4.2 基于 GHC module summary 识别可达内部模块
- [x] 4.3 使用 GHC 的依赖顺序进行确定性展平
- [x] 4.4 基于已解析的 GHC `Name` 信息区分内部引用和外部引用
- [x] 4.5 为每个可达内部模块保存 `RenamedSource` 和 `TypecheckedSource`
- [x] 4.6 直接透传 GHC 的循环依赖、缺失模块、TH 和类型检查错误，不重新实现检测逻辑

## 5. 名称变换

- [x] 5.1 创建 `Bundler.Rename`，使用基于 GHC `Name` 身份的 `NameTransform`
- [x] 5.2 根据定义模块、occurrence name 和 Haskell 词法类别生成确定性的内部名称，保持 `varid` / `conid` / `varsym` / `consym`
- [x] 5.3 将内部定义和引用重写为生成后的标识符
- [x] 5.4 收集变换后声明引用到的普通外部模块
- [x] 5.5 将普通外部引用重写为完整模块路径限定形式，不使用 alias
- [x] 5.6 检测生成名称冲突，或用确定性策略消歧
- [x] 5.7 为模块前缀生成、词法类别保持、operator fixity 处理和冲突处理添加聚焦测试

## 6. TH 与 CPP 集成

- [x] 6.1 验证 CPP 展开后的源码来自配置好的 GHC parser 管线
- [x] 6.2 验证 Template Haskell splice 在配置好的 GHC 会话中通过 typechecking 执行
- [x] 6.3 为 TH 执行失败增加诊断输出
- [x] 6.4 添加覆盖 CPP 和 Template Haskell 展开的集成 fixture

## 7. 展平候选模块与代码生成

- [x] 7.1 创建 `Bundler.Flatten`，用于组装变换后的模块段落
- [x] 7.2 保留输出声明所需的 language pragmas 和 options，并标记 `NoImplicitPrelude` / `RebindableSyntax` 等全局语义敏感扩展
- [x] 7.3 构造单个候选 `module Main (main) where` 分析模块
- [x] 7.4 为所有需要的外部模块输出 `import qualified <Full.Module.Name>`
- [x] 7.5 省略已打包内部模块的 import
- [x] 7.6 按确定性的 GHC 依赖顺序输出变换后的声明
- [x] 7.7 在候选模块中生成 `main`，调用选定可执行文件入口点的变换后绑定
- [x] 7.8 记录原始 GHC `Name`、生成标识符和候选模块声明组之间的映射
- [x] 7.9 创建 `Bundler.Output`，负责渲染候选模块和 Core pruning 后的最终源文件

## 8. Core 驱动的代码裁剪

- [x] 8.1 创建 `Bundler.Core` 或 `Bundler.DCE`，对展平候选 `Main` 模块接入 GHC Core 生成与简化管线
- [x] 8.2 通过 GHC `Target` / `targetContents` 将候选源码作为 in-memory target 加载，避免手写 `ModGuts`
- [x] 8.3 使用独立或重置后的 GHC session 分析候选 `Main`，避免与原始 executable `Main` target 冲突
- [x] 8.4 确认候选模块显式只导出 `main`，避免 Core simplifier 因模块 exports 保留所有顶层绑定
- [x] 8.5 从简化后的 Core bindings 中提取 live internal `Name` / `Id` 集合
- [x] 8.6 建立 Core live set 到候选模块生成标识符和声明组的映射
- [x] 8.7 按 Core live set 过滤 transformed Haskell 源声明和声明组
- [x] 8.8 对无法安全映射或删除后会破坏可编译性的声明给出诊断或保留策略
- [x] 8.9 添加测试验证 Core pruning 会移除未使用定义并保留入口所需定义

## 9. 错误处理

- [x] 9.1 为可执行文件选择、Cabal 配置、GHC 会话初始化、GHC load/typecheck 失败、符号冲突和输出 I/O 定义 `BundleError`
- [x] 9.2 渲染带有可操作上下文的用户可见错误信息
- [x] 9.3 保留 GHC 对 load、typecheck、CPP 和 TH 失败给出的底层诊断
- [x] 9.4 对 CLI 和 bundling 失败返回非零退出码

## 10. MVP 验证

- [x] 10.1 添加 CLI 解析单元测试
- [x] 10.2 添加确定性命名 helper 的单元测试
- [x] 10.3 添加打包 `luogu-wip` 的集成测试
- [x] 10.4 添加打包 `codeforces-wip` 的集成测试
- [x] 10.5 在原始包结构之外用 GHC 编译 MVP 生成结果
- [x] 10.6 验证生成结果解析了所有引用，只导出 `main`，不包含内部模块 import，且 Core pruning 后仍可编译
- [x] 10.7 运行现有项目测试套件

## 11. 文档

- [x] 11.1 记录 `haskell-bundler --exec <name> --output <path>` 用法
- [x] 11.2 记录 MVP 支持行为、Core pruning 行为和已知限制
- [x] 11.3 记录常见 Cabal、GHC 和 TH 失败模式
- [x] 11.4 为打包现有项目可执行文件添加示例命令

## 12. 后续阶段验证

- [x] 12.1 MVP 展平稳定后，添加完整 Template Haskell 和 CPP 集成测试
- [x] 12.2 添加 `NoImplicitPrelude` / `RebindableSyntax` / Prelude 相关 fixture，决定其支持边界
- [x] 12.3 扩展 Core live-name 到源码声明映射，覆盖更多复杂声明形态
- [x] 12.4 为 `haskell-bundler` 添加确定性的自举测试
- [x] 12.5 验证打包输出不包含时间戳、本地路径或构建环境特定值
- [ ] 12.6 打包所有项目可执行文件，并独立编译每个生成文件
