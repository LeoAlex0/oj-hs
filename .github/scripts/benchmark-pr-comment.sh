#!/usr/bin/env bash
set -euo pipefail

marker="<!-- oj-hs-benchmark-report -->"

current_csv="${BENCHMARK_CURRENT_CSV:-bench.csv}"
base_csv_override="${BENCHMARK_BASE_CSV:-}"
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
comments_json="$(mktemp)"
api_response="$(mktemp)"
api_error="$(mktemp)"
cleanup() {
  rm -f "$current_tsv" "$base_tsv" "$body_file" "$comments_json" "$api_response" "$api_error"
}
trap cleanup EXIT

print_comment_context() {
  echo "Benchmark comment context:" >&2
  echo "  repository: ${repo:-<empty>}" >&2
  echo "  pull_request: ${pr_number:-<empty>}" >&2
  echo "  pull_request_head_repository: ${pr_head_repo:-<empty>}" >&2
  echo "  current_ref: ${current_ref:-<empty>}" >&2
  echo "  base_ref: ${base_ref:-<empty>}" >&2
  echo "  benchmark_run_id: ${run_id:-<empty>}" >&2
}

print_github_api_response_body() {
  local response_file="$1"
  local body
  local status_code

  status_code="$(
    awk '
      {
        line = $0
        sub(/\r$/, "", line)
        if (line ~ /^HTTP\//) {
          code = $2
        }
      }
      END {
        print code
      }
    ' "$response_file"
  )"

  if [ -n "$status_code" ] && [ "$status_code" -lt 400 ]; then
    return
  fi

  body="$(
    awk '
      {
        line = $0
        sub(/\r$/, "", line)
        if (body) {
          print line
        }
        if (line == "") {
          body = 1
        }
      }
    ' "$response_file"
  )"

  if [ -z "$body" ]; then
    return
  fi

  echo "GitHub API response body:" >&2
  if printf '%s\n' "$body" | jq -e . >/dev/null 2>&1; then
    printf '%s\n' "$body" | jq -r '
      "  message: \(.message // "n/a")",
      "  status: \(.status // "n/a")",
      "  documentation_url: \(.documentation_url // "n/a")"
    ' >&2
  else
    printf '%s\n' "$body" | sed 's/^/  /' >&2
  fi
}

print_github_api_failure() {
  local context="$1"
  local response_file="$2"
  local error_file="$3"

  echo "GitHub API request failed while $context." >&2
  print_comment_context

  if [ -s "$response_file" ]; then
    echo "GitHub API response headers:" >&2
    awk '
      {
        line = $0
        sub(/\r$/, "", line)
        lower = tolower(line)
        if (line ~ /^HTTP\// ||
            lower ~ /^x-accepted-github-permissions:/ ||
            lower ~ /^x-github-request-id:/ ||
            lower ~ /^x-ratelimit-limit:/ ||
            lower ~ /^x-ratelimit-remaining:/ ||
            lower ~ /^x-ratelimit-resource:/ ||
            lower ~ /^x-ratelimit-reset:/) {
          print "  " line
        }
      }
    ' "$response_file" >&2
    print_github_api_response_body "$response_file"
  fi

  if [ -s "$error_file" ]; then
    echo "GitHub CLI stderr:" >&2
    sed 's/^/  /' "$error_file" >&2
  fi
}

diagnose_github_api_failure() {
  local context="$1"
  shift

  : > "$api_response"
  : > "$api_error"
  gh api --include "$@" > "$api_response" 2> "$api_error" || true
  print_github_api_failure "$context" "$api_response" "$api_error"
}

gh_api_write() {
  local context="$1"
  shift

  : > "$api_response"
  : > "$api_error"
  if gh api --include "$@" > "$api_response" 2> "$api_error"; then
    return 0
  fi

  print_github_api_failure "$context" "$api_response" "$api_error"
  return 1
}

csv_to_tsv() {
  awk -F ',' '
    NR > 1 && NF >= 2 {
      name = $1
      sub(/^All[.]/, "", name)
      mean_seconds = $2 / 1000000000000
      print name "\t" mean_seconds
    }
  ' "$1"
}

if [ ! -s "$current_csv" ]; then
  echo "Benchmark CSV not found: $current_csv" >&2
  exit 1
fi

csv_to_tsv "$current_csv" > "$current_tsv"

baseline_status="missing"
baseline_csv=""
baseline_run_id=""

mkdir -p "$base_dir"

if [ -n "$base_csv_override" ] && [ -s "$base_csv_override" ]; then
  baseline_csv="$base_csv_override"
  baseline_status="available"
  csv_to_tsv "$baseline_csv" > "$base_tsv"
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
      if [ -s "$base_dir/bench.csv" ]; then
        baseline_csv="$base_dir/bench.csv"
        baseline_status="available"
        csv_to_tsv "$baseline_csv" > "$base_tsv"
      else
        baseline_status="artifact-without-csv"
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
  echo "Lower mean time is better. Full tasty-bench CSV output is available in the \`benchmark-report\` artifact as \`bench.csv\`."
  echo

  if [ "$baseline_status" = "available" ]; then
    if [ -n "$baseline_run_id" ]; then
      echo "Baseline: [latest successful \`Benchmark\` push run on \`$base_ref\`](${server_url}/${repo}/actions/runs/${baseline_run_id})"
    else
      echo "Baseline: local benchmark CSV"
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
        gsub(/&/, "\\&amp;", s)
        gsub(/</, "\\&lt;", s)
        gsub(/>/, "\\&gt;", s)
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
        gsub(/&/, "\\&amp;", s)
        gsub(/</, "\\&lt;", s)
        gsub(/>/, "\\&gt;", s)
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

: > "$api_error"
if gh api "repos/${repo}/issues/${pr_number}/comments" --paginate > "$comments_json" 2> "$api_error"; then
  comment_id="$(
    jq -r --arg marker "$marker" '.[] | select((.body // "") | contains($marker)) | .id' "$comments_json" \
      | grep -E '^[0-9]+$' \
      | tail -n 1 || true
  )"
else
  diagnose_github_api_failure "listing benchmark PR comments" "repos/${repo}/issues/${pr_number}/comments"
  echo "Failed to list benchmark PR comments; skipping comment update to avoid duplicate comments." >&2
  exit 0
fi

if [ -n "$comment_id" ]; then
  if gh_api_write "updating benchmark PR comment #$comment_id" \
    --method PATCH \
    "repos/${repo}/issues/comments/${comment_id}" \
    -f body="$(cat "$body_file")"; then
    echo "Updated benchmark PR comment #$comment_id."
  else
    echo "Failed to update benchmark PR comment; keeping benchmark job successful." >&2
  fi
else
  if gh_api_write "creating benchmark PR comment on #$pr_number" \
    --method POST \
    "repos/${repo}/issues/${pr_number}/comments" \
    -f body="$(cat "$body_file")"; then
    echo "Created benchmark PR comment."
  else
    echo "Failed to create benchmark PR comment; keeping benchmark job successful." >&2
  fi
fi
