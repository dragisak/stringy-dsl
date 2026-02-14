#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

if [[ $# -eq 0 ]]; then
  exec sbt "run --help"
fi

cmd="run"
for arg in "$@"; do
  escaped="${arg//\\/\\\\}"
  escaped="${escaped//\"/\\\"}"
  cmd+=" \"$escaped\""
done

exec sbt "$cmd"
