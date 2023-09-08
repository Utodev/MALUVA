#!/bin/bash


sjasmplus.exe maluva_plus3.asm   --sym=symbols.a80
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
mv MLV_P3.BIN Release
