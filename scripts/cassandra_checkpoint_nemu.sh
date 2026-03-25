#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  cassandra_checkpoint_nemu.sh --workload /path/to/payload.bin [options]

Required:
  --workload PATH         Linux/BBL payload to run under NEMU for checkpointing.
                          This should boot into an environment where Cassandra is
                          already installed or auto-started.

Optional:
  --nemu-home PATH        NEMU root. Default: /raid1/menzo/xs-env/NEMU
  --output-dir PATH       Checkpoint output directory.
                          Default: /raid1/menzo/new-env/XiangShan/checkpoints/cassandra
  --name NAME             Workload name tag. Default: cassandra
  --interval N            Checkpoint interval in instructions. Default: 50000000
  --warmup N              Max instructions before stopping. Default: unset
  --mode MODE             One of: uniform, manual-uniform, manual-oneshot
                          Default: manual-uniform
  --gcpt-payload PATH     Optional payload path to link into gcpt.bin
  --with-restorer         Also pass gcpt.bin to NEMU via -r/--cpt-restorer.
                          Default: off. Leave this off for the normal manual
                          checkpoint flow on this NEMU build, otherwise the
                          restorer may overwrite the workload image in memory.
  --build-gcpt            Force rebuild gcpt_restore runtime
  --cpt-mmode             Pass --cpt-mmode to NEMU to force checkpointing in M-mode.
                          Useful when workload runs mostly in M-mode and normal
                          checkpoint eligibility checks prevent dumps.
  --checkpoint-format FMT Checkpoint format: zstd or gz. Default: zstd
  --dry-run               Print commands without executing them
  -h, --help              Show this help

Notes:
  1. This script handles the NEMU + gcpt_restore plumbing only.
  2. You still need a Cassandra-capable workload payload or disk-backed image flow.
  3. The generated .gz/.zstd checkpoints can be run with XiangShan emu via:
       ./build/emu -i /path/to/checkpoint
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

run_cmd() {
  echo "+ $*"
  if [[ "$DRY_RUN" == "0" ]]; then
    "$@"
  fi
}

NEMU_HOME="${NEMU_HOME:-/raid1/menzo/xs-env/NEMU}"
OUTPUT_DIR="/raid1/menzo/new-env/XiangShan/checkpoints/cassandra"
WORKLOAD=""
NAME="cassandra"
INTERVAL="50000000"
WARMUP=""
MODE="manual-uniform"
GCPT_PAYLOAD=""
BUILD_GCPT="0"
WITH_RESTORER="0"
CPT_MMODE="0"
DRY_RUN="0"
CHECKPOINT_FORMAT="zstd"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --workload)
      WORKLOAD="$2"
      shift 2
      ;;
    --nemu-home)
      NEMU_HOME="$2"
      shift 2
      ;;
    --output-dir)
      OUTPUT_DIR="$2"
      shift 2
      ;;
    --name)
      NAME="$2"
      shift 2
      ;;
    --interval)
      INTERVAL="$2"
      shift 2
      ;;
    --warmup)
      WARMUP="$2"
      shift 2
      ;;
    --mode)
      MODE="$2"
      shift 2
      ;;
    --gcpt-payload)
      GCPT_PAYLOAD="$2"
      shift 2
      ;;
    --build-gcpt)
      BUILD_GCPT="1"
      shift
      ;;
    --with-restorer)
      WITH_RESTORER="1"
      shift
      ;;
    --cpt-mmode)
      CPT_MMODE="1"
      shift
      ;;
    --dry-run)
      DRY_RUN="1"
      shift
      ;;
    --checkpoint-format)
      CHECKPOINT_FORMAT="$2"
      shift 2
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

if [[ -z "$WORKLOAD" ]]; then
  echo "--workload is required" >&2
  usage >&2
  exit 1
fi

case "$MODE" in
  uniform)
    MODE_FLAG="-u"
    ;;
  manual-uniform)
    MODE_FLAG="--manual-uniform-cpt"
    ;;
  manual-oneshot)
    MODE_FLAG="--manual-oneshot-cpt"
    ;;
  *)
    echo "Unsupported --mode: $MODE" >&2
    exit 1
    ;;
esac

WORKLOAD="$(realpath "$WORKLOAD")"
NEMU_HOME="$(realpath "$NEMU_HOME")"
NEMU_BIN="$NEMU_HOME/build/riscv64-nemu-interpreter"
GCPT_DIR="$NEMU_HOME/resource/gcpt_restore"
GCPT_BIN="$GCPT_DIR/build/gcpt.bin"
LOG_DIR="$OUTPUT_DIR/logs"

require_file "$WORKLOAD" "workload"
require_file "$GCPT_DIR/Makefile" "gcpt_restore Makefile"
require_file "$NEMU_HOME/Makefile" "NEMU Makefile"

mkdir -p "$OUTPUT_DIR" "$LOG_DIR"

if [[ ! -x "$NEMU_BIN" ]]; then
  echo "NEMU binary not found, building it first..."
  run_cmd make -C "$NEMU_HOME" -j"$(nproc)"
fi

if [[ "$BUILD_GCPT" == "1" || ! -f "$GCPT_BIN" ]]; then
  echo "Building gcpt_restore runtime..."
  if [[ -n "$GCPT_PAYLOAD" ]]; then
    GCPT_PAYLOAD="$(realpath "$GCPT_PAYLOAD")"
    require_file "$GCPT_PAYLOAD" "gcpt payload"
    run_cmd make -C "$GCPT_DIR" clean
    run_cmd make -C "$GCPT_DIR" NEMU_HOME="$NEMU_HOME" GCPT_PAYLOAD_PATH="$GCPT_PAYLOAD"
  else
    run_cmd make -C "$GCPT_DIR" clean
    run_cmd make -C "$GCPT_DIR" NEMU_HOME="$NEMU_HOME"
  fi
fi

require_file "$GCPT_BIN" "gcpt restore binary"

RUN_LOG="$LOG_DIR/${NAME}-${MODE}.out"
ERR_LOG="$LOG_DIR/${NAME}-${MODE}.err"

declare -a CMD
CMD+=("$NEMU_BIN")
CMD+=("$WORKLOAD")
CMD+=("-D" "$OUTPUT_DIR")
CMD+=("-w" "$NAME")
CMD+=("-C" "$NAME")
CMD+=("-b")
CMD+=("--cpt-interval" "$INTERVAL")
CMD+=("--checkpoint-format" "$CHECKPOINT_FORMAT")

if [[ "$CPT_MMODE" == "1" ]]; then
  CMD+=("--cpt-mmode")
fi

if [[ "$MODE" == "uniform" ]]; then
  CMD+=("$MODE_FLAG")
else
  if [[ "$WITH_RESTORER" == "1" ]]; then
    CMD+=("-r" "$GCPT_BIN")
  fi
  CMD+=("$MODE_FLAG")
fi

if [[ -n "$WARMUP" ]]; then
  CMD+=("-I" "$WARMUP")
fi

echo "NEMU checkpoint configuration:"
echo "  NEMU_HOME   : $NEMU_HOME"
echo "  WORKLOAD    : $WORKLOAD"
echo "  OUTPUT_DIR  : $OUTPUT_DIR"
echo "  NAME        : $NAME"
echo "  MODE        : $MODE"
echo "  INTERVAL    : $INTERVAL"
if [[ -n "$WARMUP" ]]; then
  echo "  WARMUP/MAXI : $WARMUP"
fi
if [[ "$MODE" != "uniform" && "$WITH_RESTORER" == "1" ]]; then
  echo "  GCPT_BIN    : $GCPT_BIN"
fi

echo "Logs:"
echo "  stdout -> $RUN_LOG"
echo "  stderr -> $ERR_LOG"

if [[ "$DRY_RUN" == "1" ]]; then
  printf '+ '
  printf '%q ' "${CMD[@]}"
  printf '> %q 2> %q\n' "$RUN_LOG" "$ERR_LOG"
  exit 0
fi

"${CMD[@]}" >"$RUN_LOG" 2>"$ERR_LOG"

echo
echo "Checkpoint run finished."
echo "Inspect output under: $OUTPUT_DIR"
echo "Run XiangShan on a produced checkpoint with:"
echo "  cd /raid1/menzo/new-env/XiangShan"
echo "  source /raid1/menzo/xs-env/env.sh"
echo "  ./build/emu -i /path/to/generated-checkpoint --diff ready-to-run/riscv64-nemu-interpreter-so"
