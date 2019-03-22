#!/usr/bin/env python
import argparse
import os
import sys
import shutil
import subprocess
import re
from datetime import datetime

# TSVC Categories
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

# Compiler Flags
c_flags = {
    "gcc": {
        "call": "gcc",
        "arch": {"avx2": " -march=skylake ",
                 "avx512": " -march=skylake-avx512 -mprefer-vector-width=512",
                 "knl": " -march=knl ",
                 "altivec": " -mcpu=power8 "},
        "vec": " ",
        "novec": " -fno-tree-vectorize ",
        "opt": " -O3 -ffast-math",
        "unopt": " -O0 ",
        "report": " -fopt-info-optimized=",
        "assem": " -S"
    },
    "icc": {
        "call": "icc -std=c99 -g",
        "arch": {"avx2": " -xAVX2",
                 "avx512": " -xCORE-AVX512 -qopt-zmm-usage=high",
                 "knl": " -xMIC-AVX512"},
        "vec": " ",
        "novec": " -no-simd -no-vec ",
        "opt": " -O3 -fp-model fast=2 ",
        "unopt": " -O0 ",
        "report": " -qopt-report=5 -qopt-report-file=",
        "assem": " -Fa"
    },
    "clang": {
        "call": "clang",
        "arch": {"avx2": " -march=skylake ",
                 "avx512": " -march=skylake-avx512 ",
                 "knl": " -march=knl ",
                 "altivec": " -mcpu=power8 "},
        "vec": " ",
        "novec": " -fno-vectorize ",
        "opt": " -O3 -ffast-math ",
        "unopt": " -O0 ",
        "report": " -fsave-optimization-record -foptimization-record-file=",
        "assem": " -S"
    },
    "pgi": {
        "call": " pgcc",
        "arch": {"avx2": " -tp=haswell ",
                 "avx512": " -tp=skylake ",
                 "knl": " -tp=knl ",
                 "altivec": " "},
        "vec": " -Mvect=simd ",
        "novec": " -Mnovect ",
        "opt": " -O3 -fast -fastsse",
        "unopt": " -O0 ",
        "report": " -D",
        "assem": " -S"
    },
    "ibm": {
        "call": "xlc",
        "arch": {"altivec": " "},
        "vec": " -qaltivec -qhot=vector:fastmath -qsimd=auto ",
        "novec": " -qnoaltivec -qhot=novector:fastmath -qsimd=noauto",
        "opt": " -O3",
        "unopt": " -O0 ",
        "report": " -D",
        "assem": " -S"
    }
}

# Information classes flags
parameterflags = {
    "None": " ",
    "RUNTIME_ALL": " -DRUNTIME_LOOP_BOUNDS_PARAMETERS"
                   " -DRUNTIME_ARITHMETIC_PARAMETERS"
                   " -DRUNTIME_INDEX_PARAMETERS"
                   " -DCONDITION_EVAL_PARAMETERS"
                   " -DRT_VARIABLE_ATTRIBUTES",
    "RUNTIME_LOOP_BOUNDS": " -DRUNTIME_LOOP_BOUNDS_PARAMETERS",
    "RUNTIME_ARITHMETIC": " -DRUNTIME_ARITHMETIC_PARAMETERS",
    "RUNTIME_INDEX": " -DRUNTIME_INDEX_PARAMETERS",
    "RUNTIME_CONDITIONS": " -DCONDITION_EVAL_PARAMETERS",
    "RUNTIME_ATTRIBUTES": " -DRT_VARIABLE_ATTRIBUTES",
}

# Vector ISAs
isas = {"avx2", "avx512", "knl", "altivec"}


def main():
    # Use argparse to select the appropiate benchmark set
    parser = argparse.ArgumentParser(
        description='Execute Compiler Autovectorization Benchmarks.')
    parser.add_argument('--benchmark', nargs='+', choices=benchmarks,
                        default="ALL", help="Space separated list of case "
                        "sensitive benchmark names. Allowed values are " +
                        ", ".join(benchmarks), metavar='')
    parser.add_argument('--compiler', nargs='+', choices=c_flags.keys(),
                        help="Select compiler", required=True)
    parser.add_argument('--parameters', nargs='+',
                        choices=parameterflags.keys(), help="Select compiler",
                        default="ALL")
    parser.add_argument('--isa', required=True,
                        help="Specify vector isa to test", choices=isas)
    parser.add_argument('--results', required=True,
                        help="Specify output folder")
    parser.add_argument('--source', default="../src",
                        help="Specify tsvc source location")
    parser.add_argument('--repeat', type=int, default=1,
                        help="Specify tsvc source location")
    args = parser.parse_args()

    # Select all combinations when no parameter has been selected
    if args.benchmark == "ALL":
        b_list = benchmarks
    else:
        b_list = args.benchmark

    if args.compiler == "ALL":
        c_list = c_flags.keys()
    else:
        c_list = args.compiler

    if args.parameters == "ALL":
        p_list = parameterflags.keys()
    else:
        p_list = args.parameters

    print("Executing ", b_list, "benchmarks with ", c_list, " and ", p_list)

    # Create Output folder
    basedir = "results-"+str(args.results)
    if os.path.exists(basedir):
        print("Error: ", basedir, "already exists!")
        return -1
    else:
        os.makedirs(basedir)

    # Prepare all folders with compiled tests inside
    for compiler in c_list:  # All selected compilers
        compiler_dir = os.path.join(basedir, args.isa + "-" + compiler)
        print("Creating ", compiler_dir, " folder")
        os.makedirs(compiler_dir)

        # Store platform information
        cmd = "lscpu"
        p = subprocess.Popen(cmd, cwd=compiler_dir,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE, shell=True)
        platform_out, platform_err = p.communicate()

        cmd = c_flags[compiler]['call'] + " --version"
        p = subprocess.Popen(cmd, cwd=compiler_dir,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE, shell=True)
        compiler_out, compiler_err = p.communicate()

        with open(os.path.join(compiler_dir, 'info.txt'), 'w') as f:
            f.write("Platform:\n" + platform_out.decode("utf-8") + "\n")
            f.write("Compiler:\n" + compiler_out.decode("utf-8") + "\n")

        for category in b_list:  # All selected benchmarks/categories
            category_dir = os.path.join(compiler_dir, category)
            print("Creating ", category_dir, " folder")
            os.makedirs(category_dir)

            for info in p_list:  # All selected parameters
                test_dir = os.path.join(category_dir, info)
                info_flags = parameterflags[info]
                print("Creating ", test_dir, " folder")
                os.makedirs(test_dir)

                # Copy TSVC inside the new folder
                shutil.copyfile(os.path.join(args.source, "dummy.c"),
                                os.path.join(test_dir, "dummy.c"))
                shutil.copyfile(os.path.join(args.source, "tsc_runtime.c"),
                                os.path.join(test_dir, "tsc_runtime.c"))
                shutil.copyfile(os.path.join(args.source, "parameters.dat"),
                                os.path.join(test_dir, "parameters.dat"))

                print("Compiling TSVC ", category, info)
                exec_comp(c_flags[compiler]['call']
                          + c_flags[compiler]['unopt']
                          + ' -c -o dummy.o dummy.c', test_dir)

                exec_comp(c_flags[compiler]['call']
                          + c_flags[compiler]['opt']
                          + c_flags[compiler]['vec']
                          + c_flags[compiler]['arch'][args.isa]
                          + c_flags[compiler]['report']+compiler+'_'
                          + args.isa+'_vec.txt'
                          + ' -c -o tscrtvec.o tsc_runtime.c'
                          + ' -D' + category + info_flags,
                          test_dir, 'compiler_vec.out')

                exec_comp(c_flags[compiler]['call']
                          + c_flags[compiler]['opt']
                          + c_flags[compiler]['novec']
                          + c_flags[compiler]['arch'][args.isa]
                          + c_flags[compiler]['report']+compiler+'_'
                          + args.isa+'_novec.txt'
                          + ' -c -o tscrtnovec.o tsc_runtime.c'
                          + ' -D' + category + info_flags,
                          test_dir, 'compiler_novec.out')

                # Generate assembly files
                exec_comp(c_flags[compiler]['call']
                          + c_flags[compiler]['opt']
                          + c_flags[compiler]['vec']
                          + c_flags[compiler]['arch'][args.isa]
                          + ' -S -o tscrtvec.s tsc_runtime.c'
                          + ' -D' + category + info_flags, test_dir)

                exec_comp(c_flags[compiler]['call']
                          + c_flags[compiler]['opt']
                          + c_flags[compiler]['novec']
                          + c_flags[compiler]['arch'][args.isa]
                          + ' -S -o tscrtnovec.s tsc_runtime.c'
                          + ' -D' + category + info_flags, test_dir)

                # Link  tsvc vector and scalar versions
                exec_comp(c_flags[compiler]['call']
                          + c_flags[compiler]['unopt']
                          + ' dummy.o tscrtvec.o -o runrtvec -lm', test_dir)
                exec_comp(c_flags[compiler]['call']
                          + c_flags[compiler]['unopt']
                          + ' dummy.o tscrtnovec.o -o runrtnovec -lm',
                          test_dir)

                # Run commands
                for i in range(args.repeat):
                    run_cmd(scriptdir=compiler_dir,
                            scriptname=args.isa+'-'+compiler+'_'+category,
                            testdir="/".join(test_dir.split('/')[2:]),
                            cmd='./runrtvec > runrtvec'+str(i)+'.txt')
                    run_cmd(scriptdir=compiler_dir,
                            scriptname=args.isa+'-'+compiler+'_'+category,
                            testdir="/".join(test_dir.split('/')[2:]),
                            cmd='./runrtnovec > runrtnovec'+str(i)+'.txt')


def exec_comp(cmd, test_dir, save=None):
    print("Compiling: ", cmd)
    p = subprocess.Popen(cmd, cwd=test_dir, stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE, shell=True)
    out, err = p.communicate()
    errcode = p.returncode
    if save:
        with open(os.path.join(test_dir, save), 'w') as f:
            f.write("Output: " + out.decode("utf-8"))
            f.write("Error: " + err.decode("utf-8"))


def run_cmd(scriptdir, scriptname, testdir, cmd):
    if True:  # Create a bash script with the list of benchmarks
        fname = os.path.join(scriptdir, 'runall_'+scriptname+'.sh')
        with open(fname, 'a') as f:
            f.write('cd '+testdir+'; '+cmd+'; cd -;\n')
    elif False:  # Run the benchmarks locally
        print("Executing: ", cmd)
        p = subprocess.Popen(cmd, cwd=test_dir, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE, shell=True)
        out, err = p.communicate()
        errcode = p.returncode
        print(out)
        print(err)


if __name__ == "__main__":
    main()
