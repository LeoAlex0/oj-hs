# Haskell Bundler 提案

## Why

多模块 Haskell 可执行项目很难直接提交到在线评测系统，或分发到只有最小编译环境的地方，因为可执行文件依赖原始包结构和内部模块布局。本变更引入一个 bundler，将选定的可执行文件及其可达内部模块打包成单个可独立编译的 `.hs` 文件。

## What Changes

- 新增 `haskell-bundler` CLI，用于将一个包内可执行文件打包成单个 Haskell 源文件。
- 支持通过 `--exec <name>` 选择可执行文件；未指定时默认选择包声明中的第一个可执行文件。
- 支持通过 `--output <path>` 指定输出路径；未指定时默认输出到 `bundled.hs`。
- 使用 Cabal 包元数据为选定可执行文件配置 GHC 9.6.7 会话。
- 使用 GHC 编译管线完成模块发现、import 解析、依赖排序、循环检测、CPP 展开、Template Haskell 执行、重命名和类型检查。
- 将可达内部模块展平为单个显式只导出 `main` 的 `Main` 模块。
- 基于 GHC `Name` 身份、来源模块信息和 occurrence 词法类别，对内部符号进行确定性重命名；生成名称保持 `varid` / `conid` / `varsym` / `consym` 类别，不把 operator 改成普通 identifier。
- 将普通外部包引用转换为完整模块路径限定形式，并生成无 alias 的 `import qualified <Full.Module.Name>`。
- 从一开始接入 GHC Core 简化管线：先把展平后的候选 `Main` 分析模块作为 in-memory GHC target 加载，再使用该模块的 Core live bindings 完成可达性分析和代码裁剪；源码层只负责将 Core live names 映射回可输出声明。
- 增加生成文件编译验证路径，并在后续阶段加入自举确定性验证。

## Capabilities

### New Capabilities

- `cli-interface`: 命令行可执行文件选择、输出路径选择，以及可操作的 CLI 错误信息。
- `dependency-analysis`: 基于 Cabal/GHC 的包加载、模块发现、依赖排序，以及 TH/CPP 处理。
- `symbol-transformation`: 基于 GHC `Name` 的内部重命名、外部引用限定、模块展平和冲突规避。
- `symbol-cleanup`: 基于展平候选模块的 GHC Core 简化管线、live name 提取和源码声明裁剪。
- `bootstrap-test`: 后续阶段的自打包和确定性输出验证。

### Modified Capabilities

- 无。

## Impact

- 新增 `app/bundler/` 下的可执行入口。
- 新增 `src/Bundler/` 下的 bundler 实现模块。
- 更新 `package.yaml`，加入 GHC API、Cabal 和 CLI 相关依赖，并重新生成 `oj-hs.cabal`。
- 增加 CLI 解析、命名、模块加载、展平、生成文件编译，以及后续 bootstrap 行为的测试。
- 依赖现有 Nix/Cabal 工具链和 GHC 9.6.7。
