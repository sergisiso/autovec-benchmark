#!/usr/bin/env python
import argparse
import os
import shutil
import subprocess
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

compilers = ["gcc", "icc"]


def main():
    parser = argparse.ArgumentParser(description='Execute Compiler Autovectorization Benchmarks.')

    # Select benchmark
    parser.add_argument('--benchmark', nargs='+', choices=benchmarks, default="ALL",
            help="Space separated list of case sensitive benchmark names. Allowed values are " +
            ", ".join(benchmarks), metavar='')

    parser.add_argument('--compiler', choices=compilers, help="Select compiler") 


    args = parser.parse_args()


    if args.benchmark == "ALL":
        b_list = benchmarks
    else:
        b_list = args.benchmark

    print "Executing ", b_list, "benchmarks"

    # Create Output folder
    timestamp = datetime.now().strftime('%Y-%m-%d-%H:%M')
    basedir = "results-"+timestamp
    if os.path.exists(basedir):
        print "Error: ", basedir, "already exists!"
        return -1
    else:
        os.makedirs(basedir)

    for b in b_list:
        test_dir = os.path.join(basedir,b)
        print "Creating ", test_dir , " folder"
        os.makedirs(test_dir)
        shutil.copyfile("dummy.c",os.path.join(test_dir,"dummy.c"))
        shutil.copyfile("tsc.c",os.path.join(test_dir,"tsc.c"))
        print "Compiling ", b

        cmd = 'gcc -c -o dummy.o dummy.c'
        run_cmd(cmd, test_dir)

        cmd = 'gcc -std=c99 -O3 -fivopts -flax-vector-conversions -funsafe-math-optimizations -msse4.2 -c -o tscvec.o tsc.c' + ' -D' + b
        run_cmd(cmd, test_dir)

        cmd = 'gcc -std=c99 -O3 -fivopts -flax-vector-conversions -funsafe-math-optimizations -msse4.2 -fno-tree-vectorize -c -o tscnovec.o tsc.c' + ' -D' + b
        run_cmd(cmd, test_dir)

        cmd = 'gcc -O0 dummy.o tscvec.o -o runvec -lm'
        run_cmd(cmd, test_dir)

        cmd = 'gcc -O0 dummy.o tscnovec.o -o runnovec -lm'
        run_cmd(cmd, test_dir)

        print "Run vector test", b
        run_cmd('./runvec', test_dir)
        print "Run scalar test", b
        run_cmd('./runnovec', test_dir)

def run_cmd(cmd, test_dir):
    print "Executing: ", cmd
    p = subprocess.Popen(cmd, cwd=test_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    out, err = p.communicate()
    errcode  = p.returncode
    print out
    print err



"""
flags = -std=c99 -O3 -fivopts -flax-vector-conversions -funsafe-math-optimizations -msse4.2
#flags = -std=c99 -O3 -fivopts -flax-vector-conversions -funsafe-math-optimizations -msse4.2 -Wall -Werror
vecflags = -ftree-vectorizer-verbose=1
novecflags = -fno-tree-vectorize
libs = -lm
noopt = -O0

all : runvec runnovec 

runnovec : tscnovec.o dummy.o
	$(CC) $(noopt) dummy.o tscnovec.o -o runnovec $(libs)

runvec : tscvec.o dummy.o
	$(CC) $(noopt) dummy.o tscvec.o -o runvec $(libs)

tscvec.o : tsc.c
	rm -f report.lst
	$(CC) $(flags) $(vecflags) -c -o tscvec.o tsc.c  2> reportgcc.lst

tscnovec.o : tsc.c
	$(CC) $(flags) $(novecflags) -c -o tscnovec.o tsc.c

tsc.s : tsc.c dummy.o
	$(CC) $(flags) dummy.o tsc.c -S 

dummy.o : dummy.c
	$(CC) -c dummy.c

clean :
	rm -f *.o runnovec runvec *.lst *.s
"""

if __name__ == "__main__":
    main()

