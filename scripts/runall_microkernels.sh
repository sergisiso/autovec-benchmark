# Copyright (c) 2019 Sergi Siso
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#   1. Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
#   2. Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
#   3. Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
# GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
# OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


echo "RTCT auto-vectorization test"

#module load intel/18.2.199

#module load mypgi18.4
#module load myclang6
#module load ibm/13.1.3
module load mygcc8

isa=$1
#compilers=(gnu intel pgi clang)
compilers=(gnu)
tests=(ao binomial black-scholes convolution mandelbrot matrixmult stencil)

for c in "${compilers[@]}"; do
    echo "---" > compiler_${c}_${isa}_output.txt
    echo " ############"  > output-$c-${isa}.txt
    echo " # Using compiler: $c , isa:$isa "  >> output-$c-${isa}.txt
    echo " ############" >> output-$c-${isa}.txt
    for t in "${tests[@]}"; do
        echo "#--------------" >> output-$c-${isa}.txt
        cd $t
        make clean &> /dev/null
        TEST_COMPILER=$c VECTOR_ISA=$isa make &>> ../compiler_${c}_${isa}_output.txt
        rtvec=$(./${t}.rt-vec | grep "$t serial" | awk '{print $3}')
        rtnovec=$(./${t}.rt-novec | grep "$t serial" | awk '{print $3}')
        ctvec=$(./${t}.ct-vec | grep "$t serial" | awk '{print $3}')
        ctnovec=$(./${t}.ct-novec | grep "$t serial" | awk '{print $3}')

        cd ..
        echo "$t RT NO-VEC cycles = $rtnovec" >> output-$c-${isa}.txt
        echo "$t RT VEC cycles = $rtvec" >> output-$c-${isa}.txt
        echo "$t CT NO-VEC cycles = $ctnovec" >> output-$c-${isa}.txt
        echo "$t CT VEC cycles = $ctvec"  >> output-$c-${isa}.txt
    done
done


