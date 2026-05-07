# CLI 接口规范

## ADDED Requirements

### Requirement: 用户可选择要打包的可执行文件

系统 SHALL 允许用户选择要打包的包内可执行文件。

#### Scenario: 打包指定可执行文件
- **WHEN** 用户提供 `--exec <name>`
- **THEN** 系统打包该名称对应的可执行文件及其依赖

#### Scenario: 打包默认可执行文件
- **WHEN** 用户未提供 `--exec`
- **THEN** 系统选择包描述中声明的第一个可执行文件

#### Scenario: 拒绝未知可执行文件
- **WHEN** 用户提供的可执行文件名未在包中声明
- **THEN** 系统报告该名称无效
- **AND** 系统列出所有可用的可执行文件名

### Requirement: 用户可指定输出路径

系统 SHALL 允许用户选择打包后源文件的写入位置。

#### Scenario: 写入自定义输出路径
- **WHEN** 用户提供 `--output <path>`
- **THEN** 系统将打包后的源文件写入该路径

#### Scenario: 写入默认输出路径
- **WHEN** 用户未提供 `--output`
- **THEN** 系统将打包后的源文件写入当前工作目录下的 `bundled.hs`

### Requirement: CLI 失败信息可操作

系统 SHALL 在 CLI 失败时报告足够上下文，帮助用户修正命令。

#### Scenario: 无效参数显示 usage
- **WHEN** 参数解析失败
- **THEN** 系统显示解析错误和 CLI usage 文本

#### Scenario: 报告打包失败
- **WHEN** 参数解析成功后 bundling 失败
- **THEN** 系统以失败状态退出
- **AND** 系统显示底层 bundling 错误
