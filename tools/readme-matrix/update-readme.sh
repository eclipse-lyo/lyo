#!/usr/bin/env bash

set -uo pipefail
# set -e
# set -x

SCRIPT_PATH=$(realpath "$0")
SCRIPT_DIR=$(dirname "${SCRIPT_PATH}")

trap "rm ${SCRIPT_DIR}/tmp.md" err exit SIGINT SIGTERM

if [ -x "$(command -v gsed)" ]; then
   sed_exe=gsed
else
   sed_exe=sed
fi

if ! [ -x "$(command -v sponge)" ]; then
   brew install sponge || sudo apt install moreutils || sudo zypper in moreutils
fi


pushd "${SCRIPT_DIR}"

uv run -q ./readme-matrix-generator.py downstream-projects.yml > tmp.md

# https://unix.stackexchange.com/a/737087/6317
$sed_exe -e '/<!-- END YAML TABLE -->/e cat tmp.md' -e '/<!-- BEGIN YAML TABLE -->/,//{//!d}' ../../README.md | sponge ../../README.md

popd
