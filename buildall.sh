#!/bin/bash

echo "###################### C64 ######################"
./bc64.sh
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
echo "###################### Plus/4 ######################"
./bcp4.sh
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
echo "###################### CPC ######################"
./bcpc.sh
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
echo "###################### ESXDOS ######################"
./besxdos.sh
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
echo "###################### MSX ######################"
./bmsx.sh
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
echo "###################### NEXT ######################"
./bnext.sh
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
echo "###################### PCW ######################"
./bpcw.sh
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
echo "###################### PLUS3 ######################"
./bplus3.sh
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
echo "###################### ZXUNO ######################"
./bzxuno.sh
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
echo "###################### SC2DAAD ######################"
./bsc2daad.sh
echo "####################################################"
echo "OK. Maluva compiled."
