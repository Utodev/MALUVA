#!/bin/bash

BIN/64tass.exe -a maluva_cp4.asm -o MLV_CP4.BIN -b --long-branch --labels=symbols.a64
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
mv MLV_CP4.BIN Release
