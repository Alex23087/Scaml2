#!/usr/bin/env bash

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
SCAML="$SCRIPT_DIR/../src/_build/default/bin/main_interpreter.exe"

cd "$SCRIPT_DIR/../src"
make build > /dev/null
cd "$SCRIPT_DIR"
"$SCAML" "$SCRIPT_DIR/main.scaml"
