#!/usr/bin/env bash
set -euo pipefail

# 100M-instruction Cassandra checkpoint replay, no timeout by default.
# Extra args are forwarded to the base script.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_SCRIPT="$SCRIPT_DIR/run_cassandra_ipc.sh"

if [[ ! -x "$BASE_SCRIPT" ]]; then
  echo "Missing executable base script: $BASE_SCRIPT" >&2
  exit 1
fi

exec "$BASE_SCRIPT" --max-instr 1000000 --timeout 0 "$@"
