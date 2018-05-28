#!/usr/bin/env python
import argparse
import os
import sys
import shutil
import subprocess
import re
from datetime import datetime

benchmarks = [
    "LINEAR_DEPENDENCE",
    "INDUCTION_VARIABLE",
    "GLOBAL_DATA_FLOW",
    "CONTROL_FLOW",
    "SYMBOLICS",
    "STATEMENT_REORDERING",
    "LOOP_RESTRUCTURING",
    "NODE_SPLITTING",
    "EXPANSION",
    "CROSSING_THRESHOLDS",
    "REDUCTIONS",
    "RECURRENCES",
    "SEARCHING",
    "PACKING",
    "LOOP_REROLLING",
    "EQUIVALENCING",
    "INDIRECT_ADDRESSING",
    "CONTROL_LOOPS"
]

c_flags = {
    "gcc" : {
        "call": "gcc",
        "arch": {"avx2":" -march=skylake ",
                 "avx512":" -march=skylake-avx512 -mprefer-vector-width=512",
                 "knl":" -march=knl ",
                 "altivec":" -mcpu=power8 "},
        "vec" : " ",
        "novec" : " -fno-tree-vectorize ",
        "opt" : " -O3 -ffast-math",
        "unopt" : " -O0 ",
        "report" : " -fopt-info-optimized=",
        "assem" : " -S"
    },
    "icc" : {
        "call": "icc -std=c99 -g",
        "arch": {"avx2":" -xAVX2",
                "avx512":" -xCORE-AVX512 -qopt-zmm-usage=high",
                "knl":" -xMIC-AVX512"},
        "vec" : " ",
        "novec" : " -no-simd -no-vec ",
        "opt" : " -O3 -fp-model fast=2 ",
        "unopt" : " -O0 ",
        "report" : " -qopt-report=5 -qopt-report-file=",
        "assem" : " -Fa"
    },
    "clang" : {
        "call": "clang",
        "arch": {"avx2":" -march=skylake ",
                 "avx512":" -march=skylake-avx512 ",
                 "knl":" -march=knl ",
                 "altivec":" -mcpu=power8 "},
        "vec" : " ",
        "novec" : " -fno-vectorize ",
        "opt" : " -O3 -ffast-math ",
        "unopt" : " -O0 ",
        "report" : " -fsave-optimization-record -foptimization-record-file=",
        "assem" : " -S"
    },
    "pgi" : {
        "call": " pgcc",
        "arch": {"avx2":" -tp=haswell ",
                 "avx512":" -tp=skylake ",
                 "knl":" -tp=knl ",
                 "altivec":" "},
        "vec" : " -Mvect=simd ",
        "novec" : " -Mnovect ",
        "opt" : " -O3 -fast -fastsse",
        "unopt" : " -O0 ",
        "report" : " -D",
        "assem" : " -S"
    },
    "ibm" : {
        "call": "xlc",
        "arch": {"altivec":" "},
        "vec" : " -qaltivec -qhot=vector:fastmath -qsimd=auto ",
        "novec" : " -qnoaltivec -qhot=novector:fastmath -qsimd=noauto",
        "opt" : " -O3",
        "unopt" : " -O0 ",
        "report" : " -D",
        "assem" : " -S"
    }
}


parameterflags = {
    "None":" ",
    "RUNTIME_ALL" : " -DRUNTIME_LOOP_BOUNDS_PARAMETERS -DRUNTIME_ARITHMETIC_PARAMETERS -DRUNTIME_INDEX_PARAMETERS -DCONDITION_EVAL_PARAMETERS -DVARIABLE_ATTRIBUTES",
    "RUNTIME_LOOP_BOUNDS" : " -DRUNTIME_LOOP_BOUNDS_PARAMETERS",
    "RUNTIME_ARITHMETIC" : " -DRUNTIME_ARITHMETIC_PARAMETERS",
    "RUNTIME_INDEX" : " -DRUNTIME_INDEX_PARAMETERS",
    "RUNTIME_CONDITIONS" : " -DCONDITION_EVAL_PARAMETERS",
    "RUNTIME_ATTRIBUTES" : " -DVARIABLE_ATTRIBUTES",
}

isas={"avx2","avx512","knl","altivec"}

def main():
    # Use argparse to select the appropiate benchmark set
    parser = argparse.ArgumentParser(description='Execute Compiler Autovectorization Benchmarks.')
    parser.add_argument('--benchmark', nargs='+', choices=benchmarks, default="ALL",
            help="Space separated list of case sensitive benchmark names. Allowed values are " +
            ", ".join(benchmarks), metavar='')
    parser.add_argument('--compiler', nargs='+', choices=c_flags.keys(), help="Select compiler", default="ALL") 
    parser.add_argument('--parameters', nargs='+', choices=parameterflags.keys(), help="Select compiler", default="ALL") 
    parser.add_argument('--isa', required=True, help="Specify vector isa to test", choices=isas)
    parser.add_argument('--results', required=True, help="Specify output folder")
    args = parser.parse_args()


    # Select all combinations when no parameter has been selected
    if args.benchmark == "ALL": b_list = benchmarks
    else: b_list = args.benchmark

    if args.compiler == "ALL": c_list = c_flags.keys()
    else: c_list = args.compiler

    if args.parameters == "ALL": p_list = parameterflags.keys()
    else: p_list = args.parameters

    print "Executing ", b_list, "benchmarks with ", c_list, " and ", p_list


    # Create Output folder
    basedir = "results-"+str(args.results)
    if os.path.exists(basedir):
        print "Error: ", basedir, "already exists!"
        return -1
    else:
        os.makedirs(basedir)

    # Prepare all folders with compiled tests inside
    for c in c_list: # All selected compilers
        test_dir = os.path.join(basedir,c)
        print "Creating ", test_dir , " folder"
        os.makedirs(test_dir)
            
        for b in b_list: # All selected benchmarks/categories
            test_dir = os.path.join(os.path.join(basedir,c),b)
            print "Creating ", test_dir , " folder"
            os.makedirs(test_dir)

            for p in p_list: # All selected parameters
                test_dir = os.path.join(os.path.join(os.path.join(basedir,c),b),p)
                p_flags = parameterflags[p]
                print "Creating ", test_dir , " folder"
                os.makedirs(test_dir)

                # Copy TSVC inside the new folder
                shutil.copyfile("src/dummy.c",os.path.join(test_dir,"dummy.c"))
                shutil.copyfile("src/tsc_runtime.c",os.path.join(test_dir,"tsc_runtime.c"))
                shutil.copyfile("src/parameters.dat",os.path.join(test_dir,"parameters.dat"))

                print "Compiling TSVC ", b, p
                exec_comp( c_flags[c]['call']
                        + c_flags[c]['unopt']
                        + ' -c -o dummy.o dummy.c', test_dir)

                exec_comp( c_flags[c]['call']
                        + c_flags[c]['opt']
                        + c_flags[c]['vec'] 
                        + c_flags[c]['arch'][args.isa]
                        + c_flags[c]['report']+c+'_'+args.isa+'_vec.txt'
                        + ' -c -o tscrtvec.o tsc_runtime.c'
                        + ' -D' + b + p_flags, test_dir, 'compiler_vec.out')

                exec_comp( c_flags[c]['call'] 
                        + c_flags[c]['opt'] 
                        + c_flags[c]['novec']
                        + c_flags[c]['arch'][args.isa]
                        + c_flags[c]['report']+c+'_'+args.isa+'_novec.txt'
                        + ' -c -o tscrtnovec.o tsc_runtime.c'
                        + ' -D' + b + p_flags, test_dir, 'compiler_novec.out')

                # Generate assembly files
                exec_comp( c_flags[c]['call'] 
                        + c_flags[c]['opt'] 
                        + c_flags[c]['vec']  
                        + c_flags[c]['arch'][args.isa]
                        + ' -S -o tscrtvec.s tsc_runtime.c' 
                        + ' -D' + b + p_flags, test_dir)

                exec_comp( c_flags[c]['call'] 
                        + c_flags[c]['opt'] 
                        + c_flags[c]['novec'] 
                        + c_flags[c]['arch'][args.isa]
                        + ' -S -o tscrtnovec.s tsc_runtime.c' 
                        + ' -D' + b + p_flags, test_dir)

                # Link  tsvc vector and scalar versions
                exec_comp( c_flags[c]['call'] + c_flags[c]['unopt'] + ' dummy.o tscrtvec.o -o runrtvec -lm', test_dir)
                exec_comp( c_flags[c]['call'] + c_flags[c]['unopt'] + ' dummy.o tscrtnovec.o -o runrtnovec -lm', test_dir)

                # Run commands
                for i in range(5):
                    run_cmd('./runrtvec > runrtvec'+str(i)+'.txt',
                            test_dir,c+'_'+args.isa+'_'+b)
                    run_cmd('./runrtnovec > runrtnovec'+str(i)+'.txt',
                            test_dir, c+'_'+args.isa+'_'+b)


def exec_comp(cmd, test_dir, save=None):
    print "Compiling: ", cmd
    p = subprocess.Popen(cmd, cwd=test_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    out, err = p.communicate()
    errcode  = p.returncode
    if save != None:
        with open(os.path.join(test_dir,save),'w') as f:
            f.write(out)
            f.write(err)

def run_cmd(cmd, test_dir, name):
    if True:
        with open('runall_'+name+'.sh','a') as f:
            f.write('cd '+ test_dir+';'+cmd+';cd -;\n')
    elif False:
        print "Executing: ", cmd
        p = subprocess.Popen(cmd, cwd=test_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
        out, err = p.communicate()
        errcode  = p.returncode
        print out
        print err
    else:
        job = "bsub -J name -e error.out -o output.out -W 00:19 -n 1 \"" + cmd + "\""
        print "submiting job: ", job
        p = subprocess.Popen(job, cwd=test_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
        out, err = p.communicate()
        errcode  = p.returncode
        print out
        print err

if __name__ == "__main__":
    main()

