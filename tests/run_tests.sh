#!/usr/bin/env bash
#
# run_tests.sh - Build and run the ASuiteComps FPCUnit test suite.
#
# Usage:
#   ./run_tests.sh [--lazbuild <path>] [--widgetset <name>]
#
# Exit code is 0 only if the build succeeds AND all tests pass,
# so it can gate GitHub Actions jobs directly.
#
set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LAZBUILD="lazbuild"
WIDGETSET="gtk2"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --lazbuild=*) LAZBUILD="${1#*=}"; shift ;;
    --lazbuild) LAZBUILD="${2:?missing value}"; shift 2 ;;
    --widgetset=*) WIDGETSET="${1#*=}"; shift ;;
    --widgetset) WIDGETSET="${2:?missing value}"; shift 2 ;;
    -h|--help) echo "Usage: $0 [--lazbuild <path>] [--widgetset <name>]"; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; exit 2 ;;
  esac
done

# GUI control tests need a display server on Linux.
# If none is available, re-exec under xvfb-run when present.
if [[ -z "${DISPLAY:-}" ]] && [[ "$(uname -s)" == "Linux" ]] && command -v xvfb-run >/dev/null 2>&1; then
  echo "No DISPLAY found, re-running under xvfb-run..."
  exec xvfb-run -a "$0" --lazbuild="$LAZBUILD" --widgetset="$WIDGETSET"
fi

echo "=== Building ASuiteCompsTests (widgetset=${WIDGETSET}) ==="
"$LAZBUILD" --widgetset="$WIDGETSET" "$SCRIPT_DIR/AsuiteCompsTests.lpi" || exit 1

BIN="$SCRIPT_DIR/AsuiteCompsTests"
if [[ -f "$BIN.exe" ]]; then
  BIN="$BIN.exe"
fi
if [[ ! -x "$BIN" ]]; then
  echo "Test binary not found or not executable: $BIN" >&2
  exit 1
fi

echo "=== Running ASuiteCompsTests ==="
"$BIN" --all --format=plain
