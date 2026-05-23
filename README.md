# luogu

[![CI](https://github.com/LeoAlex0/oj-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/LeoAlex0/oj-hs/actions/workflows/ci.yml)
[![Coverage](https://github.com/LeoAlex0/oj-hs/actions/workflows/coverage.yml/badge.svg)](https://github.com/LeoAlex0/oj-hs/actions/workflows/coverage.yml)
[![Benchmark](https://github.com/LeoAlex0/oj-hs/actions/workflows/benchmark.yml/badge.svg)](https://github.com/LeoAlex0/oj-hs/actions/workflows/benchmark.yml)

## 流水线产物 / Pipeline Artifacts

CI 以仓库中的 Nix flake 作为 Haskell 工具链的唯一来源。要查看某个
commit 对应的产物，请打开 GitHub Actions 中对应的 workflow run。

CI uses the repository Nix flake as the source of truth for the Haskell
toolchain. To inspect artifacts for a specific commit, open the corresponding
workflow run in GitHub Actions.

- 覆盖率结果由 Coverage workflow 上传为 `coverage-report` artifact，其中包含
  `coverage.txt` 和生成的 HPC HTML 报告。
- Coverage results are uploaded by the Coverage workflow as the
  `coverage-report` artifact, including `coverage.txt` and the generated HPC
  HTML report.
- Benchmark 结果由 Benchmark workflow 上传为 `benchmark-report` artifact，其中
  包含 `bench.html` 和 `bench.json`。Pull request 在 GitHub 授权允许评论时会
  自动更新 benchmark 评论；如果目标分支还没有 benchmark artifact，评论会只展示
  当前 PR 的 benchmark 结果，不生成 diff。
- Benchmark results are uploaded by the Benchmark workflow as the
  `benchmark-report` artifact, including `bench.html` and `bench.json`.
  Pull requests also receive an updated benchmark comment when GitHub grants
  comment permission. If the target branch has no benchmark artifact yet, the
  comment reports the current PR benchmark without a diff.
