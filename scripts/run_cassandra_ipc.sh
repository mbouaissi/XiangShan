#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  run_cassandra_ipc.sh [options]

Purpose:
  Replay a Cassandra checkpoint with the stable XiangShan flow and print IPC.

Options:
  --image PATH         Checkpoint image (.zstd or .gz)
                       Default: /raid1/menzo/new-env/benchmarks/cassandra/checkpoints/cassandra-ipc-success/cassandra-ipc-success/50000000/_50000002_memory_.zstd
  --gcpt PATH          gcpt restore binary
                       Default: /raid1/menzo/xs-env/NEMU/resource/gcpt_restore/build/gcpt.bin
  --diff PATH          difftest reference shared object
                       Default: ready-to-run/riscv64-nemu-interpreter-so
  --max-instr N        Max guest instructions (-I)
                       Default: 100000
  --timeout SEC        Kill run after timeout seconds
                       Default: 0 (no timeout)
  --log-file PATH      Log file path
                       Default: /tmp/cassandra-ipc-run.log
  --no-diff            Disable difftest
  --dry-run            Print command only
  -h, --help           Show help

Examples:
  # Default run with 100000 instructions
  bash scripts/run_cassandra_ipc.sh

  # Longer window
  bash scripts/run_cassandra_ipc.sh --max-instr 500000

  # Use a different checkpoint
  bash scripts/run_cassandra_ipc.sh --image /path/to/_xxxx_memory_.zstd
EOF
}

require_file() {
  local path="$1"
  local desc="$2"
  if [[ ! -f "$path" ]]; then
    echo "Missing ${desc}: $path" >&2
    exit 1
  fi
}

ROOT_DIR="/raid1/menzo/new-env/XiangShan"
ENV_SH="/raid1/menzo/xs-env/env.sh"
IMAGE="/raid1/menzo/new-env/benchmarks/cassandra/checkpoints/cassandra-ipc-success/cassandra-ipc-success/50000000/_50000002_memory_.zstd"
GCPT="/raid1/menzo/xs-env/NEMU/resource/gcpt_restore/build/gcpt.bin"
DIFF="ready-to-run/riscv64-nemu-interpreter-so"
MAX_INSTR="100000"
TIMEOUT_SEC="0"
LOG_FILE="/tmp/cassandra-ipc-run.log"
USE_DIFF="1"
DRY_RUN="0"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --image)
      IMAGE="$2"
      shift 2
      ;;
    --gcpt)
      GCPT="$2"
      shift 2
      ;;
    --diff)
      DIFF="$2"
      shift 2
      ;;
    --max-instr)
      MAX_INSTR="$2"
      shift 2
      ;;
    --timeout)
      TIMEOUT_SEC="$2"
      shift 2
      ;;
    --log-file)
      LOG_FILE="$2"
      shift 2
      ;;
    --no-diff)
      USE_DIFF="0"
      shift
      ;;
    --dry-run)
      DRY_RUN="1"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

require_file "$ENV_SH" "env script"
require_file "$IMAGE" "checkpoint image"
if [[ "$USE_DIFF" == "1" ]]; then
  require_file "$GCPT" "gcpt restore binary"
fi

cd "$ROOT_DIR"
# shellcheck disable=SC1091
source "$ENV_SH"

require_file "./build/emu" "emu binary"
if [[ "$USE_DIFF" == "1" ]]; then
  require_file "$DIFF" "difftest reference"
fi

CMD=("./build/emu" "-i" "$IMAGE" "-I" "$MAX_INSTR")
if [[ "$USE_DIFF" == "1" ]]; then
  CMD+=("-r" "$GCPT" "--diff" "$DIFF")
fi

echo "Replay configuration:"
echo "  image      : $IMAGE"
echo "  max-instr  : $MAX_INSTR"
echo "  timeout(s) : $TIMEOUT_SEC"
echo "  log-file   : $LOG_FILE"
if [[ "$USE_DIFF" == "1" ]]; then
  echo "  gcpt       : $GCPT"
  echo "  diff       : $DIFF"
else
  echo "  diff       : disabled"
fi

printf '+ '
printf '%q ' "${CMD[@]}"
printf '\n'

if [[ "$DRY_RUN" == "1" ]]; then
  exit 0
fi

set +e
if [[ "$TIMEOUT_SEC" != "0" ]]; then
  timeout "$TIMEOUT_SEC" stdbuf -oL -eL "${CMD[@]}" | tee "$LOG_FILE"
  RC=${PIPESTATUS[0]}
else
  stdbuf -oL -eL "${CMD[@]}" | tee "$LOG_FILE"
  RC=${PIPESTATUS[0]}
fi
set -e

echo
echo "Exit code: $RC"
echo "IPC summary:"
grep -E "instrCnt =|IPC =|EXCEEDING CYCLE/INSTR LIMIT|HIT GOOD TRAP|ABORT|BAD TRAP" "$LOG_FILE" | tail -n 20 || true

echo
echo "Full log: $LOG_FILE"
exit "$RC"
