#!/usr/bin/env python
import argparse
import os
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

c_flags = {"gcc" : {
                "vec" : " -march=native ",
                "novec" : " -fno-tree-vectorize ",
                "common" : " gcc -std=c99 -O3 -fivopts -flax-vector-conversions -funsafe-math-optimizations -ffast-math -fassociative-math",
                "report" : " -ftree-vectorizer-verbose=5 ",
                "unopt" : " gcc -O0 "
            },
            "icc" : {
                "vec" : " -xHost ",
                "novec" : " -no-simd -no-vec ",
                "common" : " icc -std=c99 -Ofast -fp-model fast=2 -prec-sqrt -ftz -fma ",
                "report" : " -qopt-report=5 ",
                "unopt" : " icc -O0 "
            },
	    "clang" : {
                "vec" : " -march=native ",
                "novec" : " -fno-vectorize ",
                "common" : " clang -std=c99 -O3 ",
                "report" : " ",
                "unopt" : " clang -O0 "
            }
}


parameterflags = {
    "None":" ",
    "RUNTIME_ALL" : " -DRUNTIME_LOOP_BOUNDS_PARAMETERS -DRUNTIME_ARITHMETIC_PARAMETERS -DRUNTIME_INDEX_PARAMETERS -DCONDITION_EVAL_PARAMETERS",
    "RUNTIME_LOOP_BOUNDS" : " -DRUNTIME_LOOP_BOUNDS_PARAMETERS",
    "RUNTIME_ARITHMETIC" : " -DRUNTIME_ARITHMETIC_PARAMETERS",
    "RUNTIME_INDEX" : " -DRUNTIME_INDEX_PARAMETERS",
    "RUNTIME_CONDITIONS" : " -DCONDITION_EVAL_PARAMETERS",
}


def main():
    # Use argparse to select the appropiate benchmark set
    parser = argparse.ArgumentParser(description='Execute Compiler Autovectorization Benchmarks.')
    parser.add_argument('--benchmark', nargs='+', choices=benchmarks, default="ALL",
            help="Space separated list of case sensitive benchmark names. Allowed values are " +
            ", ".join(benchmarks), metavar='')
    parser.add_argument('--compiler', nargs='+', choices=c_flags.keys(), help="Select compiler", default="ALL") 
    parser.add_argument('--parameters', nargs='+', choices=parameterflags.keys(), help="Select compiler", default="ALL") 
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
    timestamp = datetime.now().strftime('%Y-%m-%d-%H:%M')
    basedir = "results-"+timestamp
    if os.path.exists(basedir):
        print "Error: ", basedir, "already exists!"
        return -1
    else:
        os.makedirs(basedir)

    for c in c_list:
        test_dir = os.path.join(basedir,c)
        print "Creating ", test_dir , " folder"
        os.makedirs(test_dir)
            
        for b in b_list:
            test_dir = os.path.join(os.path.join(basedir,c),b)
            print "Creating ", test_dir , " folder"
            os.makedirs(test_dir)
            shutil.copyfile("dummy.c",os.path.join(test_dir,"dummy.c"))
            shutil.copyfile("tsc.c",os.path.join(test_dir,"tsc.c"))

            print "Compiling tsc ", b
            exec_comp(c_flags[c]['unopt'] + ' -c -o dummy.o dummy.c', test_dir)
            exec_comp(c_flags[c]['common'] + c_flags[c]['vec'] + c_flags[c]['report'] +' -c -o tscvec.o tsc.c' + ' -D' + b, test_dir)
            exec_comp(c_flags[c]['common'] + c_flags[c]['vec'] +' -S -o tscvec.s tsc.c' + ' -D' + b, test_dir)
            exec_comp(c_flags[c]['common'] + c_flags[c]['novec'] +' -c -o tscnovec.o tsc.c' + ' -D' + b, test_dir)
            exec_comp(c_flags[c]['common'] + c_flags[c]['novec'] +' -S -o tscnovec.s tsc.c' + ' -D' + b, test_dir)
            exec_comp(c_flags[c]['unopt'] + ' dummy.o tscvec.o -o runvec -lm', test_dir)
            exec_comp(c_flags[c]['unopt'] + ' dummy.o tscnovec.o -o runnovec -lm', test_dir)
     
            print "Run vector tsc", b
            run_cmd('./runvec > runvec.txt', test_dir)
            print "Run scalar tsc", b
            run_cmd('./runnovec > runnovec.txt', test_dir)


            for p in p_list:
                test_dir = os.path.join(os.path.join(os.path.join(basedir,c),b),p)
                p_flags = parameterflags[p]
                print "Creating ", test_dir , " folder"
                os.makedirs(test_dir)
                shutil.copyfile("dummy.c",os.path.join(test_dir,"dummy.c"))
                shutil.copyfile("tsc_runtime.c",os.path.join(test_dir,"tsc_runtime.c"))
                shutil.copyfile("parameters.dat",os.path.join(test_dir,"parameters.dat"))

                print "Compiling tsc_runtime ", b, p
                exec_comp(c_flags[c]['unopt'] + ' -c -o dummy.o dummy.c', test_dir)
                exec_comp(c_flags[c]['common'] + c_flags[c]['vec'] + c_flags[c]['report'] +
                    ' -c -o tscrtvec.o tsc_runtime.c' + ' -D' + b + p_flags, test_dir, 'compilervec_output.txt')
                exec_comp(c_flags[c]['common'] + c_flags[c]['vec']  + ' -S -o tscrtvec.s tsc_runtime.c' + ' -D' + b + p_flags, test_dir)
                exec_comp(c_flags[c]['common'] + c_flags[c]['novec'] +' -c -o tscrtnovec.o tsc_runtime.c' + ' -D' + b + p_flags, test_dir)
                exec_comp(c_flags[c]['common'] + c_flags[c]['novec'] +' -S -o tscrtnovec.s tsc_runtime.c' + ' -D' + b + p_flags, test_dir)
                exec_comp(c_flags[c]['unopt'] + ' dummy.o tscrtvec.o -o runrtvec -lm', test_dir)
                exec_comp(c_flags[c]['unopt'] + ' dummy.o tscrtnovec.o -o runrtnovec -lm', test_dir)

                print "Run tsc_runtime", p , "vector test", b
                run_cmd('./runrtvec > runrtvec.txt', test_dir)
                print "Run rsc_runtime ", p, "scalar test", b
                run_cmd('./runrtnovec > runrtnovec.txt', test_dir)


def exec_comp(cmd, test_dir, save=None):
    print "Compiling: ", cmd
    p = subprocess.Popen(cmd, cwd=test_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    out, err = p.communicate()
    errcode  = p.returncode
    if save != None:
        with open(os.path.join(test_dir,save),'w') as f:
            f.write(out)
            f.write(err)

def run_cmd(cmd, test_dir):
    if False:
        print "No job submitted, just compiling"
    elif True:
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

