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


#module load intel/18.2.199

#module load mypgi18.4
#module load myclang6
#module load ibm/13.1.3
#module load mygcc8

isa=avx2
#compilers=(gnu intel pgi clang)
compilers=(intel)
#tests=(matrixmult)
tests=(black-scholes convolution mandelbrot stencil lattice_boltzmann matrixmult)

for c in "${compilers[@]}"; do
    echo "---" > compiler_${c}_${isa}_output.txt
    echo " ############"  | tee output-$c-${isa}.txt
    echo " # Using compiler: $c , isa:$isa "  | tee -a output-$c-${isa}.txt
    echo " ############" | tee -a output-$c-${isa}.txt
    for t in "${tests[@]}"; do
        echo "#--------------" | tee -a output-$c-${isa}.txt
        cd $t
        make clean &> /dev/null
        make compileall TEST_COMPILER=$c VECTOR_ISA=$isa \
            &>> ../compiler_${c}_${isa}_output.txt
        rtvec=$(make runrt | grep "$t serial" | awk '{print $3}')
        #rtnovec=$(./${t}.rt-novec | grep "$t serial" | awk '{print $3}')
        ctvec=$(make runct | grep "$t serial" | awk '{print $3}')
        #ctnovec=$(./${t}.ct-novec | grep "$t serial" | awk '{print $3}')
        rtdoping=$(make rundoping | grep "$t serial" | awk '{print $3}')

        cd ..
        #echo "$t RT NO-VEC cycles = $rtnovec" | tee -a output-$c-${isa}.txt
        echo "$t RT VEC = $rtvec" | tee -a output-$c-${isa}.txt
        #echo "$t CT NO-VEC cycles = $ctnovec" | tee -a output-$c-${isa}.txt
        echo "$t CT VEC = $ctvec"  | tee -a output-$c-${isa}.txt
        echo "$t DOPING VEC = $rtdoping"  | tee -a output-$c-${isa}.txt
    done
done


