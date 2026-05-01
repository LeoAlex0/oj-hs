# 自举测试规范

## ADDED Requirements

### Requirement: Bundler 可以打包自身

系统 SHALL 支持一个 bootstrap 测试：将 bundler 可执行文件打包、编译，并用编译出的可执行文件进行第二轮打包。

#### Scenario: 创建第一轮自打包输出
- **WHEN** 原始 bundler 可执行文件以 `--exec haskell-bundler` 运行
- **THEN** 它为 bundler 可执行文件生成一个独立 Haskell 源文件

#### Scenario: 编译打包后的源码
- **WHEN** 第一轮自打包源文件通过 GHC 编译
- **THEN** 生成的可执行文件可以运行 bundler CLI

#### Scenario: 使用编译出的 bundler 再次打包
- **WHEN** 编译出的 bundled executable 再次打包 `haskell-bundler`
- **THEN** 它生成第二个独立 Haskell 源文件

### Requirement: Bootstrap 输出具有确定性

系统 SHALL 对等价输入生成稳定的打包源码。

#### Scenario: 比较 bootstrap 输出
- **WHEN** 原始 bundler 和 bundled bundler 使用相同选项处理同一目标
- **THEN** 两个生成源码文件 byte-for-byte 相同

#### Scenario: 保持确定性排序
- **WHEN** 系统输出 modules、declarations、imports 或 generated names
- **THEN** 输出顺序在多次运行之间保持确定

#### Scenario: 避免 bundler 额外引入构建环境特定输出
- **WHEN** 系统生成打包源码
- **THEN** 输出不包含 bundler 自身分析、候选模块加载或临时文件处理额外引入的 timestamps、temporary paths、local store paths 或其他构建环境特定值
- **AND** 如果用户源码、CPP 或 Template Haskell 展开结果本身明确生成环境相关值，系统保留该语义结果而不擅自清洗或替换

### Requirement: Bootstrap 测试是后续阶段验证目标

系统 SHALL 将完整自举测试视为最终验证，而不是 MVP blocker。

#### Scenario: Bootstrap 之前先完成 MVP 验证
- **WHEN** MVP bundler 可以将简单项目可执行文件打包为可编译的独立文件
- **THEN** bootstrap 测试可以暂时保持 pending

#### Scenario: 完成前运行 bootstrap 验证
- **WHEN** bundler 被认为已完成本 change
- **THEN** 完整 bootstrap cycle 通过
