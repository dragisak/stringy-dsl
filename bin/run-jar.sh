#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CP_FILE="$ROOT_DIR/target/runtime-classpath.txt"
MAIN_CLASS="mydsl.Cli"

find_jar() {
  find "$ROOT_DIR/target" -type f -name 'stringy-dsl_*.jar' | sort | tail -n 1
}

build_jar_if_missing() {
  local jar_path
  jar_path="$(find_jar || true)"
  if [[ -z "${jar_path}" ]]; then
    echo "Jar not found. Running sbt package..." >&2
    (cd "$ROOT_DIR" && sbt -batch package >/dev/null)
  fi
}

write_runtime_classpath() {
  local cp_line
  cp_line="$(
    cd "$ROOT_DIR"
    sbt -batch "export Runtime / fullClasspath" | awk '/^\// { cp = $0 } END { if (cp == "") exit 1; print cp }'
  )"
  printf '%s\n' "$cp_line" > "$CP_FILE"
}

main() {
  build_jar_if_missing

  local jar_path
  jar_path="$(find_jar || true)"
  if [[ -z "${jar_path}" ]]; then
    echo "Unable to locate packaged jar under $ROOT_DIR/target" >&2
    exit 1
  fi

  if [[ ! -f "$CP_FILE" || "$jar_path" -nt "$CP_FILE" ]]; then
    echo "Generating runtime classpath..." >&2
    write_runtime_classpath
  fi

  local classpath classes_dir
  classpath="$(<"$CP_FILE")"
  classes_dir="$ROOT_DIR/target/scala-2.13/classes"
  classpath="${classpath//$classes_dir/$jar_path}"

  if [[ $# -eq 0 ]]; then
    set -- --help
  fi

  exec java -cp "$classpath" "$MAIN_CLASS" "$@"
}

main "$@"
