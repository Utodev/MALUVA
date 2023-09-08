#!/bin/bash

sjasmplus.exe maluva_uno.asm --sym=symbols.a80
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
mv MLV_UNO*.BIN Release
