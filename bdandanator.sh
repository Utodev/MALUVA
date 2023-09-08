#!/bin/bash

#Compilando Maluva
sjasmplus.exe maluva_dandanator.asm   --sym=symbols.a80
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
mv MLV_DAN.BIN Release
