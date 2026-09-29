#!/usr/bin/env bash
# Scaling sweep runner. Resumable: cells with a result file are skipped.
#
#   ./run_all.sh          # Part A then Part B
#   ./run_all.sh A        # component sweep only
#   ./run_all.sh B        # end-to-end runs only
#
# Environment:
#   PERF_TIMEOUT_A  per-cell limit for Part A, seconds   (default 1200)
#   PERF_TIMEOUT_B  per-run limit for Part B, seconds    (default 3600)
#   PERF_MEM_GB     per-cell virtual-memory cap, Linux    (default 75% of RAM)
#   PERF_RESULTS    results directory                     (default ./results)
#   PERF_SMOKE=1    tiny grid for a quick end-to-end check
set -u
cd "$(dirname "$0")"
HERE=$PWD
PART=${1:-all}
RESULTS=${PERF_RESULTS:-$HERE/results}
export PERF_RESULTS=$RESULTS
TIMEOUT_A=${PERF_TIMEOUT_A:-1200}
TIMEOUT_B=${PERF_TIMEOUT_B:-3600}
mkdir -p "$RESULTS/A" "$RESULTS/B" "$RESULTS/logs"

TO=$(command -v timeout || command -v gtimeout || true)
[ -z "$TO" ] && echo "warning: no timeout command; cells will not be time-limited"

MEM_KB=""
if [ -r /proc/meminfo ]; then
  if [ -n "${PERF_MEM_GB:-}" ]; then
    MEM_KB=$(( PERF_MEM_GB * 1024 * 1024 ))
  else
    MEM_KB=$(awk '/MemTotal/ {printf "%d", $2 * 0.75}' /proc/meminfo)
  fi
fi

# One-off record of the machine and code under test.
{
  echo "date: $(date -u +%FT%TZ)"
  echo "host: $(hostname)"
  echo "git: $(git -C "$HERE" rev-parse HEAD 2>/dev/null) ($(git -C "$HERE" rev-parse --abbrev-ref HEAD 2>/dev/null))"
  echo "cores: $(nproc 2>/dev/null || sysctl -n hw.ncpu)"
  [ -r /proc/meminfo ] && grep MemTotal /proc/meminfo
  echo "mem cap (kB): ${MEM_KB:-none}  timeouts: A=${TIMEOUT_A}s B=${TIMEOUT_B}s"
  Rscript -e 'cat(R.version.string, "\n"); print(La_library()); print(extSoftVersion()["BLAS"]);
    for (p in c("glmnet","ranger","xgboost","mlpwr","survival","pROC","DiceKriging")) cat(p, tryCatch(as.character(packageVersion(p)), error = function(e) "MISSING"), "\n")'
} > "$RESULTS/system.txt" 2>&1

run_cell() { # $1 script, $2 id, $3 timeout; echoes exit code
  local log="$RESULTS/logs/$2.log"
  (
    [ -n "$MEM_KB" ] && ulimit -v "$MEM_KB"
    if [ -n "$TO" ]; then
      "$TO" --signal=KILL "$3" Rscript "$1" "$2"
    else
      Rscript "$1" "$2"
    fi
  ) > "$log" 2>&1
  echo $?
}

status_of() { # status column of a result csv
  awk -F, 'NR == 1 { for (i = 1; i <= NF; i++) if ($i == "\"status\"") c = i }
           NR == 2 { gsub(/"/, "", $c); print $c }' "$1"
}

stub_A() { # $1 id, $2 status, $3 message
  printf '"id","status","error"\n"%s","%s","%s"\n' "$1" "$2" "$3" > "$RESULTS/A/$1.csv"
}

if [ "$PART" = "A" ] || [ "$PART" = "all" ]; then
  ids=$(Rscript grid.R A)
  total=$(echo "$ids" | wc -l | tr -d ' ')
  i=0
  for id in $ids; do
    i=$((i + 1))
    out="$RESULTS/A/$id.csv"
    [ -f "$out" ] && continue
    family=${id%_n*}
    # A smaller n for this configuration already timed out or ran out of memory.
    if grep -lqE '"(timeout|killed|oom)"' "$RESULTS/A/${family}"_n*.csv 2>/dev/null; then
      stub_A "$id" "skipped_after_failure" "smaller n timed out or ran out of memory"
      echo "[A $i/$total] $id  skipped (smaller n failed)"
      continue
    fi
    start=$(date +%s)
    code=$(run_cell cell_A.R "$id" "$TIMEOUT_A")
    secs=$(( $(date +%s) - start ))
    if [ ! -f "$out" ]; then
      if [ "$code" = "137" ] || [ "$code" = "124" ]; then
        stub_A "$id" "timeout" "exceeded ${TIMEOUT_A}s (exit $code)"
      elif grep -qiE "cannot allocate|memory" "$RESULTS/logs/$id.log"; then
        stub_A "$id" "oom" "out of memory (exit $code)"
      else
        stub_A "$id" "killed" "no result, exit $code; see logs/$id.log"
      fi
    elif grep -qi "cannot allocate" "$out"; then
      sed -i.bak '2s/"error","/"oom","/' "$out" && rm -f "$out.bak"
    fi
    echo "[A $i/$total] $id  ${secs}s  $(status_of "$out")"
  done
fi

if [ "$PART" = "B" ] || [ "$PART" = "all" ]; then
  ids=$(Rscript grid.R B)
  for id in $ids; do
    [ -f "$RESULTS/B/$id.info.rds" ] && continue
    start=$(date +%s)
    code=$(run_cell cell_B.R "$id" "$TIMEOUT_B")
    secs=$(( $(date +%s) - start ))
    if [ ! -f "$RESULTS/B/$id.info.rds" ]; then
      Rscript -e "saveRDS(list(id = '$id', status = if ($code %in% c(124, 137)) 'timeout' else 'killed',
        error = 'exit $code after ${secs}s', elapsed = $secs), '$RESULTS/B/$id.info.rds')"
    fi
    echo "[B] $id  ${secs}s  exit $code"
  done
fi

echo "Done. Summarise with: Rscript summarise.R"
