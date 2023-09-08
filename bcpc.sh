#!/bin/bash

sjasmplus.exe maluva_cpc.asm  --sym=symbols.a80
# -exp=export.asm -sym=maluva_cpp_symbols.txt
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
mv MLV_CPC.BIN MLV_CPC_INT.BIN Release

