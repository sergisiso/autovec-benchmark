#!/usr/bin/env python3

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

""" Provide statistics and plots from a TSVC results folder. """

import sys
import os
import shutil
import statistics
import argparse
from collections import defaultdict
from plotutils import *

NUM_REPS = 1
debug = False

remove_tests = []  # ['S2712']
remove_categories = []  # ['REDUCTIONS','CONTROL_FLOW','SEARCHING']

test_sets = {
    "RUNTIME_ARITHMETIC": [
        "S000", "S1112", "S453", "S452", "S242", "S2251", "S252", "S254",
        "S255", "S256", "S258", "S281", "S1281", "S291", "S292", "S2102",
        "S31111", "S317", "S4112"],

    "RUNTIME_INDEX": [
        "S111", "S1111", "S112", "S113", "S1113", "S116", "S118", "S119",
        "S1119", "S121", "S122", "S126", "S128", "S131", "S141", "S151",
        "S431", "S161", "S275", "S277", "S171", "S172", "S173", "S174",
        "S175", "S176", "S211", "S212", "S1213", "S221", "S1221", "S222",
        "S231", "S232", "S233", "S2233", "S235", "S241", "S242", "S243",
        "S244", "S1244", "S2244", "S3251", "S256", "S257", "S258", "S261",
        "S281", "S291", "S292", "S293", "S2111", "S321", "S322", "S323",
        "S351", "S352", "S353", "S421", "S422", "S423", "S424", "S4114",
        "S4116", "S4117"],

    "RUNTIME_CONDITIONS": [
        "S123", "S124", "S161", "S1161", "S162", "S271", "S272", "S273",
        "S274", "S275", "S276", "S277", "S278", "S279", "S1279", "S2710",
        "S2711", "S441", "S443", "S481", "S258", "S314", "S315", "S316",
        "S3111", "S331", "S341", "S342", "S343"]
    }


def rgb(x):
    return [float(x[0])/255, float(x[1])/255, float(x[2])/255]


palette = {
        "Avx2-Icc": rgb([67, 0, 167]),
        "Avx2-Gcc": rgb([162, 255, 105]),
        "Avx2-Pgi": rgb([255, 93, 171]),
        "Avx2-Clang": rgb([51, 161, 0]),
        "Altivec-Ibm": rgb([30, 0, 33]),
        "Altivec-Gcc": rgb([255, 166, 69]),
        "Altivec-Pgi": rgb([1, 92, 135]),
        "Altivec-Clang": rgb([1, 92, 135]),
        "Avx512-Gcc": rgb([150, 89, 0]),
        "Avx512-Icc": rgb([99, 183, 80]),
        "Avx512-Clang": rgb([121, 0, 7]),
        "Avx512-Pgi": rgb([121, 0, 7]),
        "Knl-Gcc": rgb([150, 89, 0]),
        "Knl-Icc": rgb([99, 183, 80]),
        "Knl-Clang": rgb([121, 0, 7]),
        "Knl-Pgi": rgb([121, 0, 7]),

        "Linear Dependence": rgb([218, 145, 51]),
        "Induction Variable": rgb([89, 112, 216]),
        "Global Data Flow": rgb([180, 179, 53]),
        "Control Flow": rgb([164, 91, 207]),
        "Symbolics": rgb([99, 183, 80]),
        "Statement Reordering": rgb([202, 73, 160]),
        "Loop Restructuring": rgb([78, 182, 152]),
        "Node Splitting": rgb([215, 60, 102]),
        "Expansion": rgb([85, 122, 52]),
        "Crossing Thresholds": rgb([206, 139, 203]),
        "Reductions": rgb([174, 159, 89]),
        "Recurrences": rgb([121, 96, 164]),
        "Searching": rgb([206, 77, 51]),
        "Packing": rgb([94, 155, 213]),
        "Loop Rerolling": rgb([146, 93, 39]),
        "Equivalencing": rgb([224, 122, 139]),
        "Indirect Addressing": rgb([220, 136, 102]),
    }

ALL_PARAMETERS = [
        'RUNTIME_INDEX',
        'RUNTIME_ATTRIBUTES',
        'RUNTIME_LOOP_BOUNDS',
        'None',
        'RUNTIME_ALL',
        'RUNTIME_ARITHMETIC',
        'RUNTIME_CONDITIONS'
        ]

ALL_CATEGORIES = [
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
]


def geometric_mean(arr):
    import math

    # declare product variable and initialize it to 1.
    product = 1
    n = len(arr)

    # Compute the product of all the elements in the array.
    for i in range(0, n):
        product = product * arr[i]

    # compute geometric mean through formula pow(product, 1/n) and
    # return the value to main function.
    gm = (float)(math.pow(product, (1 / n)))
    return (float)(gm)


def load_data(data, compiler, category, parameters, parameters_path,
        baseline_path, expected_path=None):

    if parameters != "RUNTIME_ALL":
        print("Just loading 'runtime all' for now!")
        sys.exit(-2)

    print("Load " + compiler + " " + category)
    # Set filenames for tsc or tsc_runtime results
    if parameters == 'original':
        vecfname = 'runvec.txt'
        novecfname = 'runnovec.txt'
    else:
        vecfname = 'runrtvec0.txt'
        novecfname = 'runrtnovec0.txt'

    # Open results files and entries to 'data'
    vecresults = []
    novecresults = []
    basevecresults = []
    basenovecresults = []

    expectedvecresults = []

    file_number = 0
    try:
        for i in range(NUM_REPS):
            file_number = i
            vecf = open(os.path.join(parameters_path, vecfname.replace('0', str(i))), 'r')
            novecf = open(os.path.join(parameters_path, novecfname.replace('0', str(i))), 'r')
            basevecf = open(os.path.join(baseline_path, vecfname.replace('0', str(i))), 'r')
            basenovecf = open(os.path.join(baseline_path, novecfname.replace('0', str(i))), 'r')
            vecresults.append([line for line in vecf.readlines() if line[0] == 'S' or
                (line.split() and line.split()[0] == "DopingRuntime:")])
            novecresults.append([line for line in novecf.readlines() if line[0] == 'S' or
                (line.split() and line.split()[0] == "DopingRuntime:")])
            basevecresults.append([line for line in basevecf.readlines() if line[0] == 'S'])
            basenovecresults.append([line for line in basenovecf.readlines() if line[0] == 'S'])
            if expected_path:
                expectedvecf = open(os.path.join(expected_path, vecfname.replace('0', str(i))), 'r')
                expectedvecresults.append([line for line in expectedvecf.readlines() if line[0] == 'S'])
    except FileNotFoundError:
        if file_number == 0:
            print("Warning, files ", os.path.join(parameters_path, vecfname),
                  " or ", os.path.join(parameters_path, novecfname),
                  " not found!")
        else:
            print("Warning: Not all files found in ", parameters_path)
        sys.exit(0)

    for indx, base in enumerate(basevecresults[0]):
        # If line starts with S it is a test
        if base[0] == 'S':
            test, _, check = base.split()

            # Skip ignored tests
            if test in remove_tests:
                print("Warning: Test'" + test + "' has been removed.")
                continue

            # Check individual test data
            overheads = []
            vectimes = []
            novectimes = []
            basevectimes = []
            basenovectimes = []
            expectedvectimes = []
            for i in range(NUM_REPS):
                basetest_novec, basenovec_time, cs1 = basenovecresults[i][indx].split()
                basetest_vec, basevec_time, cs2 = basevecresults[i][indx].split()
                if novecresults[i][indx].split()[6] == "DopingRuntime:":  # some checksums have dyn compilation
                    _, _, _, novec_time, test_novec, _, _, _, _, _, cs3 = novecresults[i][indx].split()
                else:
                    _, _, _, novec_time, test_novec, _, cs3 = novecresults[i][indx].split()
                if vecresults[i][indx].split()[6] == "DopingRuntime:":  # some checksums have dyn compilation
                    _, overhead, _, vec_time, test_vec, _, _, _, _, _, cs4 = vecresults[i][indx].split()
                else:
                    _, overhead, _, vec_time, test_vec, _, cs4 = vecresults[i][indx].split()
                if expected_path:
                    expected_test, expectedvec_time, _ = expectedvecresults[i][indx].split()
                    if test != expected_test:
                        print("Expected test mismatch!")
                        sys.exit(0)


                if (test_novec != test or test_vec != test or
                    basetest_novec != test or basetest_vec != test):
                    print("Error: Tests positions do not match", compiler,
                          category, parameters, test)
                    sys.exit(0)

                # Check that results are within tolerance.
                if ((abs(float(check) - float(cs1)) > abs(float(check) * 0.0001)) or
                    (abs(float(check) - float(cs2)) > abs(float(check) * 0.0001)) or
                    (abs(float(check) - float(cs3)) > abs(float(check) * 0.0001)) or
                    (abs(float(check) - float(cs4)) > abs(float(check) * 0.0001))):
                    print("Warning checksums differ! ", compiler,
                          category, parameters, test, check, cs1, cs2)
                    sys.exit(0)

                overheads.append(float(overhead))
                vectimes.append(float(vec_time))
                novectimes.append(float(novec_time))
                basevectimes.append(float(basevec_time))
                basenovectimes.append(float(basenovec_time))
                expectedvectimes.append(float(expectedvec_time))

            # Get the minimum in order to minimize system noise.
            overhead = statistics.mean(overheads)
            vec_time = min(vectimes)
            novec_time = min(novectimes)
            basevec_time = min(basevectimes)
            basenovec_time = min(basenovectimes)
            expectedvec_time = min(expectedvectimes)
            # vecstd = statistics.stdev(vectimes)
            # novecstd = statistics.stdev(novectimes)

            # Check that time is big enough to be significant
            if novec_time < 0.5 or vec_time < 0.5 or \
                basevec_time < 0.5 or basenovec_time < 0.5:
                print("Warning: Time too small ", test, compiler, category,
                      parameters)

            # Nested dictionary of {compiler, category, parameters, test} =
            # [doping_overhead, performance vec, performance novec, vector eff,
            #  baseline vec, baseline novec, baseline veff]
            if float(vec_time) == 0.0:
                data[compiler][category][parameters][test] = [0, 0, 0, 0, 0, 0, 0]
            else:
                data[compiler][category][parameters][test] = [
                    overhead, vec_time, novec_time, novec_time/vec_time,
                    basevec_time, basenovec_time, basenovec_time/basevec_time,
                    expectedvec_time
                    ]


def data_sanity_check(data):
    # Nested dictionary of: compiler, category, parameters, test
    print("Checking", data.keys())
    for compiler in sorted(data.keys()):
        print(compiler)
        tests_per_category = []
        total = 0

        # Check all compilers have all categories
        if len(data[compiler].keys()) != 17:
            print("Error: " + compiler + " missing categories")
            print(data[compiler].keys())
            sys.exit(-1)
        else:
            for category in data[compiler].keys():
                if category not in ALL_CATEGORIES:
                    print("Error: " + compiler + " unknown category " +
                          category)
                    sys.exit(-1)

                total += len(data[compiler][category]['RUNTIME_ALL'].keys())
                tests_per_category.append(len(
                    data[compiler][category]['RUNTIME_ALL'].keys()))

        print(compiler, total, tests_per_category)

    if True:
        # Print veff differences btw information classes
        # TODO: I can extend on this, multiple info classes,
        # does the difference comes from baseline of vector?
        print("")
        print("Tests that regress with Doping:")
        speedups = []
        regressions = 0
        same_perf = 0
        small_impr = 0
        big_impr = 0
        expected_sum = 0
        for compiler in sorted(data.keys()):
            for category in data[compiler].keys():

                found = False
                for test, results in data[compiler][category]['RUNTIME_ALL'].items():
                    # [doping_overhead, performance vec, performance novec,
                    # veff, baseline vec, baseline novec, baseline veff]
                    _, doping, _, _, baseline, novec, _, expected = results
                    speedups.append(baseline/doping)

                    if baseline/doping < 0.9:  # or novec*1.1 < baseline:
                        regressions = regressions + 1
                        found = True
                        print(test, baseline/doping, "has doping=", doping, " baseline=",
                              baseline, " novec=", novec)
                    elif baseline/doping < 1.1:
                        same_perf = same_perf + 1
                    elif baseline/doping < 2:
                        small_impr = small_impr + 1
                    else:
                        big_impr = big_impr + 1

                    if doping > expected*1.1:
                        print(test, expected/doping, "has doping=", doping, " expected=", expected)
                        expected_sum += 1

                if found:
                    print("in ", compiler, category)
                    found = False
        print("found a regression in ", regressions, "tests")
        print("Same perf in ", same_perf)
        print("Small improvement in ", small_impr)
        print("Big improvement in ", big_impr)
        print("Tests that are less than expected: ", expected_sum)
        print("Average:", statistics.mean(speedups),
              " median:", statistics.median(speedups),
              " geomean:", geometric_mean(speedups))
        print("")


def print_table(data):
    """ Print the data in tablular format"""

    for compiler in sorted(data.keys()):
        print("==============", compiler, "=============")
        print("{:<8} {:<8} {:<8} {:<8} {:<8} ".format(
            "Test", "baseline", "Doping", "Overhead", "SpeedUp"))
        lines = []
        for category in data[compiler].keys():
            print(category)
            for test, results in data[compiler][category]['RUNTIME_ALL'].items():
                # [doping_overhead, performance vec, performance novec, veff,
                #  baseline vec, baseline novec, baseline veff]
                overhead, doping, _, _, baseline, novec, _, _ = results
                print("{:<8} {:<8} {:<8} {:<8} {:<8} ".format(
                    test, baseline, doping, overhead, baseline/doping))


def getfolders(path):
    """Return the list of folders inside the given path"""
    return list(filter(lambda x: os.path.isdir(os.path.join(path, x)),
                       os.listdir(path)))


def nested_dict(level, element_type):
    """ Return a nested dictionary """
    if level == 1:
        return defaultdict(element_type)
    return defaultdict(lambda: nested_dict(level-1, element_type))


def plot_compiler(data, outputdir):
    import matplotlib.pyplot as plt
    plt.figure()

    overheads = []
    speedups = []

    compiler = [k for k in data.keys()][0]
    for category in data[compiler].keys():
        for test, results in data[compiler][category]['RUNTIME_ALL'].items():
            overhead, doping, _, _, baseline, novec, _, _ = results
            overheads.append(overhead)
            speedups.append(baseline/doping)

    boxplot = plt.boxplot(overheads)

    plt.show()
    boxplot = plt.boxplot(speedups)
    plt.show()


def main():
    """ Plotting script entry point """
    # pylint: disable=too-many-statements, too-many-branches, too-many-locals

    # Use argparse to select the appropriate benchmark set
    parser = argparse.ArgumentParser(
        description='Compare doping result folders.')
    parser.add_argument('--testdir', required=True,
                        help="Specify folder containing the TSVC doping "
                             "results.")
    parser.add_argument('--print-results', action="store_true",
                        help="Write the results to stdout")
    args = parser.parse_args()

    # Check that the results folder exist
    if not os.path.exists(args.testdir):
        print("Results folder '" + args.testdir + "' does not exist!")
        sys.exit(-2)
    baseline_folder = ''.join(args.testdir.split('dope_--'))
    if not os.path.exists(baseline_folder):
        print("Results folder '" + baseline_folder + "' does not exist!")
        sys.exit(-2)

    # Create output directory
    outputdir = "results-plots"
    if os.path.exists(outputdir):
        shutil.rmtree(outputdir)
    os.makedirs(outputdir)

    # Nested dictionary of {compiler, category, parameters, test} =
    #     [doping_overhead, performance vec, performance novec, vector eff,
    #     baseline vec, baseline novec, baseline veff]
    data = nested_dict(4, list)

    print("Loading data...")
    compiler = baseline_folder
    for category in getfolders(args.testdir):
        category_path = os.path.join(args.testdir, category)
        baseline_category_path = os.path.join(baseline_folder, category)
        parameter = 'RUNTIME_ALL'
        doping_path = os.path.join(category_path, parameter)
        baseline_path = os.path.join(baseline_category_path, parameter)
        expected_path = os.path.join(baseline_category_path, 'RUNTIME_ATTRIBUTES')
        # expected_path = os.path.join(baseline_category_path, 'None')
        load_data(data, compiler, category, parameter, doping_path,
                  baseline_path, expected_path)

    print("\nData sanity check (necessary for plots) ...")
    data_sanity_check(data)

    if args.print_results:
        print_table(data)

    if ALL_CATEGORIES in getfolders(args.testdir):
        print("Error: It should contain all categories!")
        sys.exit(-3)

    if False:
        plot_compiler(data, outputdir)


if __name__ == "__main__":
    main()
