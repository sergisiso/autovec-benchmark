
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
        echo "$t RT NO-VEC = $rtnovec" >> output-$c-${isa}.txt
        echo "$t RT VEC = $rtvec" >> output-$c-${isa}.txt
        echo "$t CT NO-VEC = $ctnovec" >> output-$c-${isa}.txt
        echo "$t CT VEC = $ctvec"  >> output-$c-${isa}.txt
    done
done


