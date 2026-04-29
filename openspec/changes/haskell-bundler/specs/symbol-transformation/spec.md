# 符号变换规范

## ADDED Requirements

### Requirement: 系统使用 GHC Names 重命名模块内部符号

系统 SHALL 基于 GHC `Name` 身份、来源模块信息和 occurrence 词法类别重命名模块内部符号。

#### Scenario: 重命名内部值定义
- **WHEN** 系统遇到由打包内部模块定义的顶层值
- **THEN** 系统基于该模块身份生成确定性的前缀名称
- **AND** 对该 `Name` 的所有内部引用都重写为同一个生成名称

#### Scenario: 重命名内部类型和类定义
- **WHEN** 系统遇到由打包内部模块定义的 type、data、newtype、type family 或 class
- **THEN** 系统基于该模块身份生成确定性的前缀名称
- **AND** 对原始 `Name` 的引用都重写为生成名称

#### Scenario: 重命名内部数据构造器和 record fields
- **WHEN** 系统遇到由打包内部模块定义的 constructor 或 record field
- **THEN** 系统基于该模块身份生成确定性的前缀名称
- **AND** 构造器和字段引用都一致重写

#### Scenario: 保持生成名称的 Haskell 词法类别
- **WHEN** 系统为内部 GHC `Name` 生成新名称
- **THEN** 生成名称保持原 occurrence 的 Haskell 词法类别：`varid` 仍为 `varid`，`conid` 仍为 `conid`，`varsym` 仍为 `varsym`，`consym` 仍为 `consym`
- **AND** 系统不得把 operator occurrence 改写为普通 identifier，即使所有使用点都以 prefix form 渲染

#### Scenario: 同步处理重命名后的 operator fixity
- **WHEN** 被重命名的内部 operator 出现在 fixity declaration 中
- **THEN** 系统将该 fixity declaration 同步指向生成后的 operator 名称
- **AND** 如果最终输出不保留该 operator 的任何 infix 使用，系统可以省略对应 fixity declaration，但候选源码仍必须可解析

### Requirement: 系统限定普通外部引用

系统 SHALL 使用完整模块路径限定普通外部包模块引用，并生成 qualified imports。

#### Scenario: 为外部模块生成 qualified import
- **WHEN** 变换后的代码引用某个 `Name`，且其定义模块未被作为内部模块打包
- **THEN** 系统为该模块输出 `import qualified <Full.Module.Name>`

#### Scenario: 限定原本非限定的外部引用
- **WHEN** 变换后的代码引用原本以非限定形式 import 的外部 `Name`
- **THEN** 系统将该引用渲染为 `<Full.Module.Name>.<symbol>`

#### Scenario: 保留完整模块名且不使用 alias
- **WHEN** 系统为外部模块生成 imports
- **THEN** 系统不发明 alias
- **AND** 所有外部引用都使用完整模块路径作为 qualifier

#### Scenario: 不依赖原始 external qualifier 或 import alias
- **WHEN** 系统渲染普通外部 `Name`
- **THEN** 系统从该 `Name` 的定义模块生成完整模块路径 qualifier
- **AND** 系统不要求 GHC pretty-printer 保留原始源码中的 external qualifier 或 import alias

#### Scenario: 区分 Prelude 和语法脱糖名字
- **WHEN** 外部 `Name` 来自 Prelude、base 隐式导入或语法脱糖所需名字
- **THEN** 系统不把该名字简单归入普通外部 qualified 引用策略
- **AND** 系统使用选定可执行文件的 GHC 配置处理该名字，或在不支持时报告明确限制

### Requirement: 系统将内部模块展平为单个候选 Main 模块

系统 SHALL 生成一个显式只导出 `main` 的候选 `Main` 模块，包含可达内部模块的变换后声明，并供 Core pruning 和最终输出使用。

#### Scenario: 生成单个候选模块头
- **WHEN** 系统生成展平候选模块
- **THEN** 候选模块包含单个 `module Main (main) where` 头部

#### Scenario: 省略已打包内部模块的 import
- **WHEN** 某个 import 指向已作为内部模块打包的模块
- **THEN** 生成输出不包含该 import
- **AND** 对该模块符号的引用使用变换后的内部名称

#### Scenario: 生成选定可执行文件入口
- **WHEN** 选定可执行文件的原始入口点已完成变换
- **THEN** 候选模块中的 `main` 调用变换后的入口绑定

#### Scenario: 记录声明组映射
- **WHEN** 系统把内部模块声明加入候选模块
- **THEN** 系统记录原始 GHC `Name`、生成标识符和候选模块声明组之间的映射
- **AND** 该映射可供 Core pruning 阶段决定保留或移除声明组

### Requirement: 系统避免展平后的符号冲突

系统 SHALL 确保生成名称在展平输出中不冲突。

#### Scenario: 检测生成名称冲突
- **WHEN** 两个不同的内部 GHC `Name` 会生成同一个标识符
- **THEN** 系统报告符号冲突，或应用确定性的消歧策略

#### Scenario: 区分外部模块符号和内部符号
- **WHEN** 某个外部模块导出的符号 occurrence name 与内部符号相同
- **THEN** qualified 引用使外部引用区别于变换后的内部标识符
