#!/bin/bash

#Compilando Maluva
sjasmplus.exe maluva_esxdos.asm   --sym=symbols.a80
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
mv MLV_ESX.BIN Release
