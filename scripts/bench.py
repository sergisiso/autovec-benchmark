#!/usr/bin/env python

# Copyright (c) 2019-2020 Sergi Siso
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

""" This application prepares the TSVC benchmark with the selected
parameters. """

import argparse
import os
import sys
import shutil
import subprocess

# TSVC Categories
BENCHMARKS = [
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
C_FLAGS = {
    "gcc": {
        "call": "gcc -DTYPE=float",
        "arch": {"avx2": " -mtune=native ",
                 "avx512": " -mtune=skylake-avx512 -mprefer-vector-width=512",
                 "knl": " -mtune=knl ",
                 "altivec": " -mtune=power8 "},
        "vec": " ",
        "novec": " -fno-tree-vectorize ",
        "opt": " -O3", # "-O0 -g ", #debugging - undo
        "unopt": " -O0 ",
        "report": " -fopt-info-optimized=",
        "assem": " -S",
        "pgo-profile": " -fprofile-generate",
        "pgo-use": " -fprofile-use="
    },
    "icc": {
        "call": "icc -std=c99 -g",
        "arch": {"avx2": " -xAVX2",
                 "avx512": " -xCORE-AVX512 -qopt-zmm-usage=high",
                 "knl": " -xMIC-AVX512"},
        "vec": " ",
        "novec": " -no-simd -no-vec ",
        "opt": " -O3",
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
        "opt": " -O3",
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
        "opt": " -O3",
        "unopt": " -O0 ",
        "report": " -D",
        "assem": " -S"
    },
    "ibm": {
        "call": "xlc",
        "arch": {"altivec": " "},
        "vec": " -qaltivec -qhot=vector -qsimd=auto ",
        "novec": " -qnoaltivec -qhot=novector -qsimd=noauto",
        "opt": " -O3",
        "unopt": " -O0 ",
        "report": " -D",
        "assem": " -S"
    }
}

# Information classes flags
PARAMETER_FLAGS = {
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
ISAS = {"avx2", "avx512", "knl", "altivec"}


def main():
    """ Benchmark application entry point """
    # pylint: disable=too-many-statements, too-many-branches, too-many-locals

    # Use argparse to select the appropriate benchmark set
    parser = argparse.ArgumentParser(
        description='Execute Compiler Autovectorization Benchmarks.')
    parser.add_argument('--benchmark', nargs='+', choices=BENCHMARKS,
                        default="ALL", help="Space separated list of case "
                        "sensitive benchmark names. Allowed values are " +
                        ", ".join(BENCHMARKS), metavar='')
    parser.add_argument('--compiler', nargs='+', choices=C_FLAGS.keys(),
                        help="Select compiler", required=True)
    parser.add_argument('--parameters', nargs='+',
                        choices=PARAMETER_FLAGS.keys(),
                        help="Select the parameters provided at run-time",
                        default="ALL")
    parser.add_argument('--isa', required=True,
                        help="Specify vector isa to test", choices=ISAS)
    parser.add_argument('--results', required=True,
                        help="Specify output folder")
    parser.add_argument('--source', default="../src",
                        help="Specify tsvc source location")
    parser.add_argument('--pgo-profile', action="store_true",
                        help="Enable PGO Profiling flags")
    parser.add_argument('--pgo-use', action="store_true",
                        help="Use PGO information to improve compilation")
    parser.add_argument('--run-locally', action="store_true",
                        help="Run each benchmark locally after they are generated")
    parser.add_argument('--generate-assembly', action="store_true",
                        help="Generate the assembly file of each test in "
                             "addition to the binary.")
    parser.add_argument('--cmd-prefix', default="",
                        help="Add a prefix before each compiler invocation")
    parser.add_argument('--repeat', type=int, default=1,
                        help="Repeat each benchmarks the specified number of"
                             "times")
    parser.add_argument('--run-novec', action="store_true",
                        help="Also execute the tests with no-vectorization flags")
    args = parser.parse_args()

    # Select all combinations when no parameter has been selected
    if args.benchmark == "ALL":
        b_list = BENCHMARKS
    else:
        b_list = args.benchmark

    if args.compiler == "ALL":
        c_list = C_FLAGS.keys()
    else:
        c_list = args.compiler

    if args.parameters == "ALL":
        p_list = PARAMETER_FLAGS.keys()
    else:
        p_list = args.parameters

    print("Executing ", b_list, "benchmarks with ", c_list, " and ", p_list)

    # Create Output folder
    basedir = "results-"+str(args.results)
    if os.path.exists(basedir):
        print("Warning: ", basedir, "already exists!")
    else:
        os.makedirs(basedir)

    # Prepare all folders with compiled tests inside
    for compiler in c_list:  # All selected compilers
        compiler_dir = os.path.join(basedir, args.isa + "-" + \
            args.cmd_prefix.replace(" ", "_")  + compiler)
        print("Creating ", compiler_dir, " folder")

        if os.path.exists(compiler_dir):
            print("Warning: ", compiler_dir, "already exists!")
        else:
            os.makedirs(compiler_dir)

            # Store platform information
            cmd = "lscpu"
            with subprocess.Popen(cmd, cwd=compiler_dir,
                                  stdout=subprocess.PIPE,
                                  stderr=subprocess.PIPE, shell=True) as process:
                platform_out, _ = process.communicate()

            cmd = C_FLAGS[compiler]['call'] + " --version"
            with subprocess.Popen(cmd, cwd=compiler_dir,
                                  stdout=subprocess.PIPE,
                                  stderr=subprocess.PIPE, shell=True) as process:
                compiler_out, compiler_err = process.communicate()

            with open(os.path.join(compiler_dir, 'info.txt'), 'w') as fout:
                fout.write("Platform:\n" + platform_out.decode("utf-8") + "\n")
                fout.write("Compiler:\n" + compiler_out.decode("utf-8") + "\n")
                fout.write("Compiler:\n" + compiler_err.decode("utf-8") + "\n")

        for category in b_list:  # All selected benchmarks/categories
            category_dir = os.path.join(compiler_dir, category)
            print("Creating ", category_dir, " folder")
            if os.path.exists(category_dir):
                print("Warning: ", category_dir, "already exists!")
            else:
                os.makedirs(category_dir)

            for info in p_list:  # All selected parameters
                test_dir = os.path.join(category_dir, info)

                pgoflags = ""
                pgoflags_vec = ""
                pgoflags_novec = ""
                if args.pgo_profile:
                    test_dir = test_dir + "-pgoprofile"
                    pgoflags_vec = C_FLAGS[compiler]['pgo-profile']
                    pgoflags_novec = C_FLAGS[compiler]['pgo-profile']
                elif args.pgo_use:
                    pgoflags_vec = C_FLAGS[compiler]['pgo-use'] + "../" \
                        + test_dir + "-pgoprofile/tscrtvec.gcda"
                    pgoflags_novec = C_FLAGS[compiler]['pgo-use'] + "../" \
                        + test_dir + "-pgoprofile/tscrtnovec.gcda"
                    test_dir = test_dir+"-pgouse"

                info_flags = PARAMETER_FLAGS[info]
                print("Creating ", test_dir, " folder")
                if os.path.exists(test_dir):
                    print("Error: ", test_dir, "already exists! Skipping!")
                    break
                os.makedirs(test_dir)

                # Copy TSVC inside the new folder
                shutil.copyfile(os.path.join(args.source, "dummy.c"),
                                os.path.join(test_dir, "dummy.c"))
                shutil.copyfile(os.path.join(args.source, "tsc_runtime.c"),
                                os.path.join(test_dir, "tsc_runtime.c"))
                shutil.copyfile(os.path.join(args.source, "parameters.dat"),
                                os.path.join(test_dir, "parameters.dat"))

                print("Compiling TSVC ", category, info)
                # Compile Dummy file
                exec_comp(args.cmd_prefix + ' ' + C_FLAGS[compiler]['call']
                          + C_FLAGS[compiler]['unopt']
                          + ' -c -o dummy.o dummy.c', test_dir)

                # Compile Vector version
                exec_comp(args.cmd_prefix + ' ' + C_FLAGS[compiler]['call']
                          + C_FLAGS[compiler]['opt']
                          + C_FLAGS[compiler]['vec']
                          + C_FLAGS[compiler]['arch'][args.isa]
                          + C_FLAGS[compiler]['report']+compiler+'_'
                          + args.isa+'_vec.txt' + pgoflags_vec
                          + ' -c -o tscrtvec.o tsc_runtime.c'
                          + ' -D' + category + info_flags,
                          test_dir, 'compiler_vec.out')

                # Compile Scalar version
                if args.run_novec:
                    exec_comp(args.cmd_prefix + ' ' + C_FLAGS[compiler]['call']
                              + C_FLAGS[compiler]['opt']
                              + C_FLAGS[compiler]['novec']
                              + C_FLAGS[compiler]['arch'][args.isa]
                              + C_FLAGS[compiler]['report']+compiler+'_'
                              + args.isa+'_novec.txt' + pgoflags_novec
                              + ' -c -o tscrtnovec.o tsc_runtime.c'
                              + ' -D' + category + info_flags,
                              test_dir, 'compiler_novec.out')

                # Generate assembly files
                if args.generate_assembly:
                    exec_comp(args.cmd_prefix + ' ' + C_FLAGS[compiler]['call']
                              + C_FLAGS[compiler]['opt']
                              + C_FLAGS[compiler]['vec']
                              + C_FLAGS[compiler]['arch'][args.isa]
                              + ' -S -o tscrtvec.s tsc_runtime.c'
                              + ' -D' + category + info_flags, test_dir)
                    exec_comp(args.cmd_prefix + ' ' + C_FLAGS[compiler]['call']
                              + C_FLAGS[compiler]['opt']
                              + C_FLAGS[compiler]['novec']
                              + C_FLAGS[compiler]['arch'][args.isa]
                              + ' -S -o tscrtnovec.s tsc_runtime.c'
                              + ' -D' + category + info_flags, test_dir)

                # Link TSVC vector and scalar versions
                exec_comp(args.cmd_prefix + ' ' + C_FLAGS[compiler]['call']
                          + C_FLAGS[compiler]['unopt'] + pgoflags
                          + ' dummy.o tscrtvec.o -o runrtvec -lm', test_dir)

                if args.run_novec:
                    exec_comp(args.cmd_prefix + ' ' + C_FLAGS[compiler]['call']
                              + C_FLAGS[compiler]['unopt'] + pgoflags
                              + ' dummy.o tscrtnovec.o -o runrtnovec -lm',
                              test_dir)

                # Run commands
                for i in range(args.repeat):
                    if args.run_locally:
                        scriptname = None
                    else:
                        scriptname = 'runall_'+args.isa+'-'+compiler+ \
                            '_'+category+'.sh'
                    run_cmd(testdir="/".join(test_dir.split('/')[2:]),
                            cmd='./runrtvec > runrtvec'+str(i)+'.txt',
                            scriptdir=compiler_dir, scriptfname=scriptname)
                    if args.run_novec:
                        run_cmd(testdir="/".join(test_dir.split('/')[2:]),
                                cmd='./runrtnovec > runrtnovec'+str(i)+'.txt',
                                scriptdir=compiler_dir, scriptfname=scriptname)


def exec_comp(cmd, test_dir, save_file=None):
    """ Execute Compiler command """
    print("Compiling: ", cmd)
    with subprocess.Popen(cmd, cwd=test_dir, stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE, shell=True) as process:
        out, err = process.communicate()
        errcode = process.returncode
    if save_file:
        with open(os.path.join(test_dir, save_file), 'w') as fout:
            fout.write("Command:\n" + cmd + "\n")
            fout.write("Output:\n" + out.decode("utf-8") + "\n")
            fout.write("Return code: " + str(errcode) + "\n")
            fout.write("Error:\n" + err.decode("utf-8") + "\n")
    if errcode != 0:
        print("Output:\n" + out.decode("utf-8"))
        print("Return code: " + str(errcode))
        print("Error:\n" + err.decode("utf-8"))
        sys.exit()


def run_cmd(testdir, cmd, scriptdir, scriptfname=None):
    """ Run benchmark locally or append run command in a script to be
    executed later. """
    if scriptfname:  # Create a bash script with the list of benchmarks
        scriptfile = os.path.join(scriptdir, scriptfname)
        with open(scriptfile, 'a') as fout:
            fout.write('cd '+testdir+'; '+cmd+'; cd -;\n')
    else:  # Run the benchmarks locally
        print("Executing: ", cmd)
        run_dir = os.path.join(scriptdir, testdir)
        with subprocess.Popen(cmd, cwd=run_dir, stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE, shell=True) as process:
            out, err = process.communicate()
            errcode = process.returncode
        print("Output:\n" + out.decode("utf-8"))
        print("Return code: " + str(errcode))
        print("Error:\n" + err.decode("utf-8"))


if __name__ == "__main__":
    main()
