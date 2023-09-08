#!/bin/bash

fpc.exe SC2DAAD
mv SC2DAAD.exe Release        
fpc SC2DAAD
mv SC2DAAD Release
rm *.o
