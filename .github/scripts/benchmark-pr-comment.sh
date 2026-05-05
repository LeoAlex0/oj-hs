#!/usr/bin/env bash
set -euo pipefail

marker="<!-- oj-hs-benchmark-report -->"

current_json="${BENCHMARK_CURRENT_JSON:-bench.json}"
base_json_override="${BENCHMARK_BASE_JSON:-}"
base_dir="${BENCHMARK_BASE_DIR:-artifacts/benchmark-base}"
summary_file="${BENCHMARK_SUMMARY:-benchmark-summary.md}"

repo="${GITHUB_REPOSITORY:-}"
base_ref="${BASE_REF:-}"
current_ref="${CURRENT_REF:-}"
run_id="${GITHUB_RUN_ID:-}"
server_url="${GITHUB_SERVER_URL:-https://github.com}"
sha="${GITHUB_SHA:-unknown}"
pr_number="${PR_NUMBER:-}"
pr_head_repo="${PR_HEAD_REPO:-}"
run_url="${server_url}/${repo}/actions/runs/${run_id}"

current_tsv="$(mktemp)"
base_tsv="$(mktemp)"
body_file="$(mktemp)"
cleanup() {
  rm -f "$current_tsv" "$base_tsv" "$body_file"
}
trap cleanup EXIT

if [ ! -s "$current_json" ]; then
  echo "Benchmark JSON not found: $current_json" >&2
  exit 1
fi

jq -r '.[2][] | [.reportName, .reportAnalysis.anMean.estPoint] | @tsv' \
  "$current_json" > "$current_tsv"

baseline_status="missing"
baseline_json=""
baseline_run_id=""

mkdir -p "$base_dir"

if [ -n "$base_json_override" ] && [ -s "$base_json_override" ]; then
  baseline_json="$base_json_override"
  baseline_status="available"
  jq -r '.[2][] | [.reportName, .reportAnalysis.anMean.estPoint] | @tsv' \
    "$baseline_json" > "$base_tsv"
elif [ -n "$repo" ] && [ -n "$base_ref" ] && [ -n "${GH_TOKEN:-}" ]; then
  baseline_run_id="$(
    gh run list \
      --repo "$repo" \
      --workflow benchmark.yml \
      --branch "$base_ref" \
      --event push \
      --status success \
      --limit 1 \
      --json databaseId \
      --jq '.[0].databaseId // empty' 2>/dev/null || true
  )"

  if [ -n "$baseline_run_id" ]; then
    if gh run download "$baseline_run_id" \
      --repo "$repo" \
      --name benchmark-report \
      --dir "$base_dir" >/dev/null 2>&1; then
      if [ -s "$base_dir/bench.json" ]; then
        baseline_json="$base_dir/bench.json"
        baseline_status="available"
        jq -r '.[2][] | [.reportName, .reportAnalysis.anMean.estPoint] | @tsv' \
          "$baseline_json" > "$base_tsv"
      else
        baseline_status="artifact-without-json"
      fi
    else
      baseline_status="artifact-missing"
    fi
  fi
fi

current_count="$(wc -l < "$current_tsv" | tr -d ' ')"
short_sha="${sha:0:12}"

{
  echo "## Benchmark"
  echo
  echo "Commit: \`$short_sha\`"
  if [ -n "$current_ref" ] || [ -n "$base_ref" ]; then
    echo "Branch: \`${current_ref:-unknown}\` -> \`${base_ref:-unknown}\`"
  fi
  echo "Run: [Benchmark workflow artifacts]($run_url)"
  echo
  echo "Lower mean time is better. Full Criterion output is available in the \`benchmark-report\` artifact as \`bench.html\` and \`bench.json\`."
  echo

  if [ "$baseline_status" = "available" ]; then
    if [ -n "$baseline_run_id" ]; then
      echo "Baseline: [latest successful \`Benchmark\` push run on \`$base_ref\`](${server_url}/${repo}/actions/runs/${baseline_run_id})"
    else
      echo "Baseline: local benchmark JSON"
    fi
    echo
    echo "| Benchmark | Base mean | PR mean | Diff |"
    echo "| --- | ---: | ---: | ---: |"
    awk -F '\t' '
      function fmt(s) {
        if (s < 0.000001) {
          return sprintf("%.2f ns", s * 1000000000)
        }
        if (s < 0.001) {
          return sprintf("%.2f us", s * 1000000)
        }
        if (s < 1) {
          return sprintf("%.2f ms", s * 1000)
        }
        return sprintf("%.2f s", s)
      }

      function md(s) {
        gsub(/\|/, "\\|", s)
        return s
      }

      NR == FNR {
        base[$1] = $2
        next
      }

      {
        name = $1
        current = $2
        if (name in base && base[name] > 0) {
          diff = ((current - base[name]) / base[name]) * 100
          printf("| %s | %s | %s | %+.2f%% |\n", md(name), fmt(base[name]), fmt(current), diff)
        } else {
          printf("| %s | n/a | %s | new |\n", md(name), fmt(current))
        }
      }
    ' "$base_tsv" "$current_tsv"
  else
    echo "> No baseline benchmark artifact was found for target branch \`$base_ref\`. This is expected for a fresh PR or a branch whose Benchmark workflow has not produced artifacts yet."
    echo
    echo "| Benchmark | PR mean |"
    echo "| --- | ---: |"
    awk -F '\t' '
      function fmt(s) {
        if (s < 0.000001) {
          return sprintf("%.2f ns", s * 1000000000)
        }
        if (s < 0.001) {
          return sprintf("%.2f us", s * 1000000)
        }
        if (s < 1) {
          return sprintf("%.2f ms", s * 1000)
        }
        return sprintf("%.2f s", s)
      }

      function md(s) {
        gsub(/\|/, "\\|", s)
        return s
      }

      {
        printf("| %s | %s |\n", md($1), fmt($2))
      }
    ' "$current_tsv"
  fi

  echo
  echo "_Benchmarks reported: $current_count._"
} > "$summary_file"

if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  cat "$summary_file" >> "$GITHUB_STEP_SUMMARY"
fi

if [ -z "$pr_number" ]; then
  echo "No pull request number is available; benchmark summary was written only to the job summary."
  exit 0
fi

if [ "$pr_head_repo" != "$repo" ]; then
  echo "Pull request comes from $pr_head_repo; skipping PR comment because forked PR tokens are read-only."
  exit 0
fi

{
  echo "$marker"
  echo
  cat "$summary_file"
} > "$body_file"

comment_id="$(
  gh api "repos/${repo}/issues/${pr_number}/comments" \
    --paginate \
    --jq ".[] | select(.body | contains(\"$marker\")) | .id" 2>/dev/null \
    | tail -n 1 || true
)"

if [ -n "$comment_id" ]; then
  if gh api \
    --method PATCH \
    "repos/${repo}/issues/comments/${comment_id}" \
    -f body="$(cat "$body_file")" >/dev/null; then
    echo "Updated benchmark PR comment #$comment_id."
  else
    echo "Failed to update benchmark PR comment; keeping benchmark job successful." >&2
  fi
else
  if gh api \
    --method POST \
    "repos/${repo}/issues/${pr_number}/comments" \
    -f body="$(cat "$body_file")" >/dev/null; then
    echo "Created benchmark PR comment."
  else
    echo "Failed to create benchmark PR comment; keeping benchmark job successful." >&2
  fi
fi
