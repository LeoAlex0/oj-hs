# 依赖分析规范

## ADDED Requirements

### Requirement: 系统通过 Cabal 和 GHC 初始化分析环境

系统 SHALL 使用 Cabal 包元数据，为选定可执行文件配置 GHC 会话。

#### Scenario: 加载包配置
- **WHEN** 系统开始打包某个包目录
- **THEN** 系统读取 Cabal 包描述和选定 executable stanza
- **AND** 系统使用该可执行文件的 source directories、依赖、扩展和 compiler options 配置 GHC

#### Scenario: 报告包配置错误
- **WHEN** Cabal 包配置无法加载或解析
- **THEN** 系统报告 Cabal 错误，且不退回到手写包发现逻辑

#### Scenario: 保存候选分析所需的 GHC 配置
- **WHEN** 系统完成选定可执行文件的 GHC 会话初始化
- **THEN** 系统保存与该可执行文件一致的 package、extension、compiler option 和 optimization 配置
- **AND** 后续候选 `Main` in-memory target 使用同一组配置进行 parse、typecheck、desugar 和 Core 简化

### Requirement: GHC 发现模块和依赖

系统 SHALL 使用 GHC 编译管理器发现模块、imports、依赖边、顺序和 import 循环。

#### Scenario: 通过 GHC load 发现模块
- **WHEN** 系统将选定可执行模块设置为 GHC target 并调用 `load`
- **THEN** GHC 使用已配置的 source directories 发现可达模块
- **AND** 系统读取生成的 `ModuleGraph`

#### Scenario: 通过 GHC 解析 imports
- **WHEN** GHC 加载某个可达模块
- **THEN** GHC 将 textual imports 解析为内部模块或外部包模块
- **AND** 系统从 GHC summaries 和 resolved names 中读取 import 信息

#### Scenario: 使用 GHC 排序
- **WHEN** 存在多个可达内部模块
- **THEN** 系统使用 GHC 的模块图顺序进行确定性展平

#### Scenario: 透传 import 循环错误
- **WHEN** GHC 报告循环模块依赖
- **THEN** 系统报告 GHC 诊断，而不是运行独立的循环检测器

### Requirement: 系统捕获 renamed 和 typechecked 模块数据

系统 SHALL 将 GHC parser、renamer 和 typechecker 的输出作为后续 bundling 阶段的事实来源。

#### Scenario: 捕获 renamed source
- **WHEN** GHC 成功 rename 某个可达内部模块
- **THEN** 系统保存该模块的 `RenamedSource`
- **AND** 后续符号变换使用 GHC `Name` 信息，而不是文本启发式

#### Scenario: 捕获 typechecked source
- **WHEN** GHC 成功 typecheck 某个可达内部模块
- **THEN** 系统保存该模块的 `TypecheckedSource`
- **AND** 后续 Core 简化阶段可使用 typechecker 结果生成 Core 并提取 live bindings

### Requirement: GHC 展开 TH 和 CPP

系统 SHALL 依赖 GHC 在正常解析和类型检查过程中处理 Template Haskell 与 CPP。

#### Scenario: 展开 Template Haskell splices
- **WHEN** GHC typecheck 包含 Template Haskell splice 的模块
- **THEN** GHC 按配置好的会话执行 splice
- **AND** 系统使用 GHC 展开后的输出进行 bundling

#### Scenario: 展开 CPP 条件分支
- **WHEN** GHC parse 包含 CPP 条件分支的模块
- **THEN** GHC 在生成后续 AST 前运行 CPP
- **AND** 系统使用 CPP 之后的模块表示

#### Scenario: 透传 TH 执行失败
- **WHEN** GHC 执行 Template Haskell splice 失败
- **THEN** 系统向用户报告 GHC 诊断
