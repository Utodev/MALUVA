#!/bin/bash

BIN/64tass.exe -a maluva_c64.asm -o MLV_C64.BIN -b --long-branch --labels=symbols.a64
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi

mv MLV_C64.BIN Release
