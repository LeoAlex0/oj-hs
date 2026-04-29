# 符号清理规范

## ADDED Requirements

### Requirement: 系统使用展平候选模块的 GHC Core 简化管线识别 live bindings

系统 SHALL 通过展平候选 `Main` 模块的 GHC Core 简化管线完成可达性分析，而不是实现自定义 source-level 可达性遍历。

#### Scenario: 构造候选分析模块并生成 Core
- **WHEN** GHC 已成功 load 并 typecheck 选定可执行文件及其可达内部模块
- **THEN** 系统构造展平后的候选 `module Main (main) where` 分析模块
- **AND** 系统通过 GHC in-memory target 对该候选模块运行 parse、typecheck、desugar 和 Core 简化管线
- **AND** 候选模块只导出 `main`，避免把所有顶层绑定作为模块 exports 保留

#### Scenario: 提取 Core live bindings
- **WHEN** Core 简化管线完成
- **THEN** 系统从简化后的 Core bindings 中提取仍然 live 的内部 `Name` 或 `Id`
- **AND** 该 live set 成为后续候选模块生成标识符和源码声明组裁剪的事实来源

#### Scenario: 透传 Core pipeline 失败
- **WHEN** 候选分析模块构造、GHC Core 生成、简化或依赖加载失败
- **THEN** 系统向用户报告 GHC 诊断

### Requirement: 系统按 Core live set 裁剪源码声明

系统 SHALL 使用 Core live set 决定哪些打包源码声明进入最终输出。

#### Scenario: 保留 Core 标记为 live 的声明
- **WHEN** 某个源码声明组关联的生成标识符出现在 Core live set 中
- **THEN** 系统在生成输出中保留该声明组

#### Scenario: 移除 Core 未标记为 live 的声明
- **WHEN** 某个源码声明组关联的生成标识符不在 Core live set 中
- **AND** 该声明组可以安全地作为独立可裁剪单元
- **THEN** 系统从生成输出中省略该声明组

#### Scenario: 以声明组为裁剪粒度
- **WHEN** GHC 或源码表示将若干声明组织为相互依赖的声明组
- **THEN** 系统根据声明组内是否存在 live `Name` 或 live 生成标识符决定保留或移除整个声明组

### Requirement: 系统不将 Core 反编译为 Haskell 源码

系统 SHALL 将 Core 用作可达性和裁剪事实来源，但最终输出仍由变换后的 Haskell 源声明生成。

#### Scenario: Core 仅驱动裁剪
- **WHEN** 系统生成最终 bundled source
- **THEN** 系统输出 Haskell 源声明，而不是 Core 语法
- **AND** Core live set 只用于决定源码声明组是否保留

#### Scenario: 无法安全映射的声明保持可编译
- **WHEN** 某个源码声明组无法可靠映射到 Core live names，或删除它会破坏生成源码的可编译性
- **THEN** 系统报告该映射限制或保留该声明组以保证输出可编译
