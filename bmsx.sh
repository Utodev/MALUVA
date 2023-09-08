#!/bin/bash


# "Compilando extern y msxdaad"
sjasmplus.exe maluva_msx.asm   --sym=symbols.a80
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
sjasmplus.exe msxdos_loader.asm
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
mv msxdaad.com MLV_MSX.BIN Release

