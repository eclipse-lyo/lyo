#!/usr/bin/env bash

set -uo pipefail
set -e
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
   if [ -x "$(command -v brew)" ]; then
      brew install sponge
   elif [ -x "$(command -v apt)" ]; then
      sudo apt install moreutils
      brew install sponge
   elif [ -x "$(command -v zypper)" ]; then
      sudo zypper in moreutils
   fi
fi


pushd "${SCRIPT_DIR}"

uv run -q ./readme-matrix-generator.py downstream-projects.yml > tmp.md

# https://unix.stackexchange.com/a/737087/6317
$sed_exe -e '/<!-- END YAML TABLE -->/e cat tmp.md' -e '/<!-- BEGIN YAML TABLE -->/,//{//!d}' ../../README.md | sponge ../../README.md

popd
