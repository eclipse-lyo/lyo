#!/usr/bin/env bash
set -euo pipefail

duration="${1:-90s}"

JAZZER_FUZZ=1 mvn -B \
  -pl core/oslc-query \
  -am \
  -Dtest=QueryFuzzTest \
  -Dsurefire.failIfNoSpecifiedTests=false \
  -Djazzer.max_duration="${duration}" \
  test -P'!spotbugs'
