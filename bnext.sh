#!/bin/bash

#Compilando Maluva
sjasmplus.exe --zxnext maluva_next.asm   --sym=symbols.a80
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
mv MLV_NEXT.BIN Release
