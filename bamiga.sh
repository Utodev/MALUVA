#!/bin/bash

BIN/vasmm68k_std.exe -m68000 -no-opt -Fbin -o MLV_AMIGA.BIN maluva_amiga.asm
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
mv MLV_AMIGA.BIN Release

