#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
    echo "Just 1 argument expected (assembly file)"
    exit -1
fi

nvec=$(grep -E "vaddps|vmulps|vfmadd[0-9]*ps|vfmsub[0-9]*ps" $1 | wc -l)
n512=$(grep -E "vaddps|vmulps|vfmadd[0-9]*ps|vfmsub[0-9]*ps" $1 | grep zmm | wc -l)

grep -E "vaddps|vmulps|vfmadd[0-9]*ps|vfmsub[0-9]*ps|vexp2ps" $1
echo " ------------------- "
echo " ------------------- "
echo " ------------------- "
grep "zmm" $1
echo "SIMD instructions: $nvec ($n512 with 512-len)"


