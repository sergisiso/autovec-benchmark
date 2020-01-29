#!/usr/bin/env python3

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


import sys
import os
import shutil
import math
import statistics
from collections import defaultdict
import numpy as np
import matplotlib.pyplot as plt
from plotutils import *

debug = False

remove_tests = ['S2712']
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

all_parameters = [
        'RUNTIME_INDEX',
        'RUNTIME_ATTRIBUTES',
        'RUNTIME_LOOP_BOUNDS',
        'None',
        'RUNTIME_ALL',
        'RUNTIME_ARITHMETIC',
        'RUNTIME_CONDITIONS'
        ]

all_categories = [
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


def load_data(data, compiler, category, parameters, parameters_path):

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
    file_number = 0
    try:
        for i in range(5):
            file_number = i
            vecf = open(os.path.join(parameters_path,
                        vecfname.replace('0', str(i))), 'r')
            novecf = open(os.path.join(parameters_path,
                          novecfname.replace('0', str(i))), 'r')
            vecresults.append(vecf.readlines())
            novecresults.append(novecf.readlines())
    except FileNotFoundError:
        if file_number == 0:
            print("Warning, files ", os.path.join(parameters_path, vecfname),
                  " or ", os.path.join(parameters_path, novecfname),
                  " not found!")
        else:
            print("Warning: Not all files found in ", parameters_path)
        exit(0)

    for indx, base in enumerate(vecresults[0]):
        # If line starts with S it is a test
        if base[0] == 'S':
            test, exetime, cs = base.split()

            # Skip ignored tests
            if test in remove_tests:
                continue

            # Check individual test data
            vectimes = []
            novectimes = []
            for i in range(5):
                test_novec, novec_time, cs1 = novecresults[i][indx].split()
                test_vec, vec_time, cs2 = vecresults[i][indx].split()

                if test_novec != test or test_vec != test:
                    print("Error: Tests possitions do not much", compiler,
                          category, parameters, test)
                    exit(0)

                # Check that results are within tolerance.
                if (abs(float(cs) - float(cs1)) > abs(float(cs) * 0.01)) \
                   or (abs(float(cs) - float(cs2)) > abs(float(cs) * 0.01)):
                    print("Warning checksums differ! ", compiler,
                          category, parameters, test, cs, cs1, cs2)
                    exit(0)

                vectimes.append(float(vec_time))
                novectimes.append(float(novec_time))

            # Get the minimum in order to minimize system noise.
            vec_time = min(vectimes)
            novec_time = min(novectimes)
            # vecstd = statistics.stdev(vectimes)
            # novecstd = statistics.stdev(novectimes)

            # Check that time is big enough to be significant
            if float(novec_time) < 0.1 or float(vec_time) < 0.1:
                print("Warning: Time too small ",
                      str([novec_time, vec_time]), test, compiler,
                      category, parameters)

            data[compiler][category][parameters][test] = [
                float(vec_time),
                float(novec_time),
                float(novec_time)/float(vec_time)
                ]


def load_microkernels(data, path, archcomp):
    architecture = archcomp.split('-')[0]
    compiler = archcomp.split('-')[1]
    folder = os.path.join(path, 'MICROKERNELS')
    with open(os.path.join(folder, 'summary.txt'), 'r') as f:
        for line in f:
            # If it is a comment line, skip it
            if(line[0] == '#' or line[1] == '#'):
                continue
            # stencil RT NO-VEC cycles = [1964.311]
            test = str(line.split()[0])
            execution = str(line.split()[1]) + " " + str(line.split()[2])
            if len(line.split()) >= 6:
                value_str = line.split()[5]
                if value_str[0] == '[':
                    value_str = value_str[1:-2]
                value = float(value_str)
                data[test][architecture][compiler][execution] = value
            else:
                data[test][architecture][compiler][execution] = 'Error'
                print("Error, could not find value for ", compiler,
                      architecture, test, execution)


def data_sanity_check(data):
    # Nested dictionary of: compiler, category, parameters, test
    for compiler in sorted(data.keys()):
        tests_per_category = []
        total = 0

        # Check all compilers have all categories
        if len(data[compiler].keys()) != 17:
            print("Error: " + compiler + " missing categories")
            print(data[compiler].keys())
            exit(0)
        else:
            for category in data[compiler].keys():
                if category not in all_categories:
                    print("Error: " + compiler + " unknown category " +
                          category)
                    exit(0)

        for category in data[compiler].keys():
            if len(data[compiler][category].keys()) != 7:
                print("Error: " + compiler + " " + category +
                      " missing parameters")
                print(data[compiler][category].keys())
                exit(0)
            else:
                for parameter in data[compiler][category].keys():
                    if parameter not in all_parameters:
                        print("Error: " + compiler + " " + category +
                              " missing parameters")
                        exit(0)

            for parameter in data[compiler][category].keys():
                if data[compiler][category][parameter].keys() != \
                   data[compiler][category]['None'].keys():
                    print("Error: missmatching parameters " + compiler +
                          " " + category + " " + parameter)
                    print(data[compiler][category][parameter].keys())
                    print(data[compiler][category]['None'].keys())
                    exit(0)
                if len(data[compiler][category][parameter].keys()) < 1:
                    print("Error: Empty " + compiler + " " + category +
                          " " + parameter)
                    exit(0)

            total = total + len(data[compiler][category]['None'].keys())
            tests_per_category.append(len(
                data[compiler][category]['None'].keys()))

        print(compiler, total, tests_per_category)

    if False:  # Print veff differences btw information classes
        # TODO: I can extend on this, multiple info classes,
        # does the difference comes from baseline of vector?
        print("")
        print("Tests which improve vector eff with less information:")
        for compiler in sorted(data.keys()):
            for category in data[compiler].keys():
                par1 = 'None'
                par2 = 'RUNTIME_CONDITIONS'
                tests = data[compiler][category][par1].keys()

                for t in tests:
                    par1veff = data[compiler][category][par1][t][2]
                    par2veff = data[compiler][category][par2][t][2]
                    if (par1veff + 4) < par2veff:
                        print(t, compiler, category)
                        print("    All exposed:" +
                              str(data[compiler][category][par1][t]))
                        print("    Cond. hidden:" +
                              str(data[compiler][category][par2][t]))


def plot_parameter(data, parameter, output, title=""):
    # Lists to store plot data
    labels = []
    parameter_exposed = []
    parameter_hidden = []

    for compiler in data.keys():
        labels.append(compiler)

        compiler_exposed = []
        compiler_hidden = []
        for category in data[compiler].keys():

            category_exposed = []
            category_hidden = []

            for test, value in data[compiler][category]['None'].items():
                value_exposed = value[2]
                try:
                    rt = data[compiler][category][parameter][test]
                except KeyError:
                    print("Error: " + compiler + " " + category + " " +
                          parameter + " " + test + " does not exist.")
                    exit(0)
                value_hidden = rt[2]

                if debug and (value_hidden > value_exposed + 0.1):
                    print("Warrning: Test " + str(test) +
                          " has better veff with RT")
                    print("CT: vec " + str(value[0]) + " novec " +
                          str(value[1]) + " veff " + str(value_exposed))
                    print("RT: vec " + str(rt[0]) + " novec " +
                          str(rt[1]) + " veff " + str(value_hidden))

                if value_hidden == 0.0 or value_exposed == 0.0:
                    print("Error: " + str(test) + " contains 0")
                    exit(0)

                category_exposed.append(value_exposed)
                category_hidden.append(value_hidden)

            category_geomean_exposed = geometric_mean(category_exposed)
            category_geomean_hidden = geometric_mean(category_hidden)

            compiler_exposed.append(category_geomean_exposed)
            compiler_hidden.append(category_geomean_hidden)

        # Aggregate platform-compiler pair results
        compiler_geomean_exposed = geometric_mean(compiler_exposed)
        compiler_geomean_hidden = geometric_mean(compiler_hidden)
        parameter_exposed.append(compiler_geomean_exposed)
        parameter_hidden.append(compiler_geomean_hidden)

    char = ('Known at\nCompile Time', 'Hidden at\nCompile Time')
    vals = [parameter_exposed, parameter_hidden]
    labels = [x.title() for x in labels]

    print("\nParameter: ", parameter)
    for i in range(len(labels)):
        variation = (parameter_exposed[i] / parameter_hidden[i] * 100) - 100
        print("{} ({:3.2f}, {:3.2f}): {:3.1f}".format(
            labels[i],
            parameter_exposed[i],
            parameter_hidden[i],
            round(variation, 1)
            ))

    plot_vspectrum(
        char, labels, vals, output, title=title,
        ylabel="Vector Efficiency GeoMean", connect=True, ymin=1, ymax=2.5)


def plot_max_info_architecture(data, output, architecture, title=""):

    # All compilers, all categories, original tsc
    values = []
    labels = []

    compiler_list = [x for x in data.keys() if architecture in x]
    category_list = [x for x in all_categories if x not in remove_categories]

    for comp in compiler_list:
        values_category = []
        for cat in category_list:
            # TODO: What should be the baseline? Is None ok? add PGO?
            par = 'None'
            values_category.append(geometric_mean(
                [x[2] for x in data[comp][cat][par].values()]))

        values.append(values_category)

    labels = [x.title().replace("_", " ") for x in category_list]
    char = [x.split("-")[1] for x in compiler_list]

    if True:
        for idx, compiler in enumerate(char):
            print("\nMax info", architecture, compiler, "analysis")
            for lab, val in sorted(zip(labels, values[idx]),
                                   key=lambda x: x[1]):
                print("{} : {:3.1f}".format(lab, round(val, 2)))
            print("GeoMean=", round(geometric_mean(values[idx]), 1),
                  "  Mean=", round(statistics.mean(values[idx]), 1),
                  "  Median=", round(statistics.median(values[idx]), 1))

    # Plot using Vector Spectrum charts
    # path = os.path.join(
    #     os.path.join('plots', 'categories_maxinfo_vspectrum'), output)
    # plot_vspectrum(
    #    char, labels, values, path, title=title, ylabel="Vector Efficiency",
    #    connect=False, draw_mean=True, size=(5, 4))

    # order with clang-avx2 increasing vector efficiencies.
    ordered_labels = [
        'Recurrences', 'Statement Reordering', 'Searching',
        'Packing', 'Loop Restructuring', 'Node Splitting',
        'Crossing Thresholds', 'Loop Rerolling',
        'Indirect Addressing', 'Linear Dependence', 'Expansion',
        'Control Flow', 'Equivalencing', 'Induction Variable',
        'Global Data Flow', 'Symbolics', 'Reductions']
    ordered_values = []
    for idx, compiler in enumerate(char):
        newl = [x[1] for x in
                sorted(zip(labels, values[idx]),
                       key=lambda x: ordered_labels.index(x[0]))]
        ordered_values.append(newl)

    # Plot using bars (ordered)
    path = os.path.join(
       os.path.join('plots', 'categories_maxinfo_bars'), output)
    barslabels = []
    barvalues = []
    barchars = []
    for idx2, compiler in enumerate(char):
        barcat = []
        barchars.append(compiler)
        for idx1, label in enumerate(ordered_labels):
            barslabels.append(label+'-'+compiler)
            barcat.append(ordered_values[idx2][idx1])
        barvalues.append(barcat)
    plot_bars(ordered_labels, barvalues[0], barvalues[1], barvalues[2],
              barvalues[3], path, title, barchars,
              size=(12, 4), longversion=True)


def plot_categories(data, comp, output, title=""):
    print(title)
    labels = []
    vals = []
    char = ('None',
            'RUNTIME_ATTRIBUTES',
            'RUNTIME_INDEX',
            'RUNTIME_CONDITIONS',
            'RUNTIME_LOOP_BOUNDS',
            'RUNTIME_ALL')

    chars_labels = (
        'Evrything\nexposed to\nthe compiler',
        'Indices\nparameters\nhidden',
        'Arithmetic\nparameters\nhidden',
        'Loop\nbounds\nhidden',
        'Variable\nattributes\nhidden',
        'All\ninformation\nhidden')

    for cat in [x for x in data[comp].keys() if x not in remove_categories]:
        labels.append(cat)
        vals2 = []
        for par in char:
            # print(par, "->" , data[comp][cat].keys())
            vals2.append(geometric_mean(
                [x[2] for x in data[comp][cat][par].values()]))
        vals.append(vals2)

    vals = [list(i) for i in zip(*vals)]
    labels = [x.title().replace("_", " ") for x in labels]

    plot_vspectrum(char, labels, vals, output, title=title,
                   ylabel="Vector Efficiency", connect=False,
                   draw_mean=True, size=(7, 4), ymax=8)


def plot_kernel(kerneldata, output, title="Empty"):

    labels = []
    sp_rt_novec = []
    sp_rt_vec = []
    sp_ct_novec = []
    sp_ct_vec = []

    for arch, archdict in kerneldata.items():
        for compiler, compdict in archdict.items():
            def compute_speedup(d, execution):
                if d[execution] == 'Error':
                    return 0.0
                else:
                    return d['RT NO-VEC']/d[execution]

            labels.append(arch+'-'+compiler)
            sp_rt_novec.append(compute_speedup(compdict, 'RT NO-VEC'))
            sp_rt_vec.append(compute_speedup(compdict, 'RT VEC'))
            sp_ct_novec.append(compute_speedup(compdict, 'CT NO-VEC'))
            sp_ct_vec.append(compute_speedup(compdict, 'CT VEC'))

    plot_bars(labels, sp_rt_novec, sp_rt_vec, sp_ct_novec, sp_ct_vec,
              output, title, chars=('hidden_novec', 'hidden_vec',
                                    'exposed_novec', 'exposed_vec'))


def print_summary(data):
    categories = all_categories

    # Potentially change names and order
    parameters = ['None', 'RUNTIME_ATTRIBUTES', 'RUNTIME_INDEX',
                  'RUNTIME_LOOP_BOUNDS', 'RUNTIME_CONDITIONS', 'RUNTIME_ALL']

    mappars = {'None': 'All info',
               'RUNTIME_ATTRIBUTES': 'Attributes h.',
               'RUNTIME_INDEX': 'Indices h.',
               'RUNTIME_LOOP_BOUNDS': 'L. Bounds h.',
               'RUNTIME_CONDITIONS': 'Conditions h.',
               'RUNTIME_ALL': 'All info h.'}

    rotation = 90
    cat_num = len(categories)
    par_num = len(parameters)

    fname = os.path.join(os.path.join('plots', 'latex_table'), 'output.tex')
    with open(fname, 'w') as f:
        f.write("\\documentclass{article}\n")
        f.write("\\usepackage[left=1cm]{geometry}\n")
        f.write("\\usepackage{array,longtable}\n")
        f.write("\\usepackage{multicol}\n")
        f.write("\\usepackage{multirow}\n")
        f.write("\\begin{document}\n")
        f.write("\\small\n")
        f.write("\\setlength\\tabcolsep{3pt}")

        # Begin table 1 with altivec and avx2
        f.write("\\begin{longtable}{")
        f.write(("|p{2cm}"+"|c"*(17))+"|}\n")
        f.write("\\cline{3-18} \\multicolumn{2}{c|}{}")
        f.write(" & \\multicolumn{4}{|c|}{ Altivec (on Power8)}")
        f.write(" & \\multicolumn{4}{|c|}{ AVX2 (on Skylake)}")
        f.write(" & \\multicolumn{4}{|c|}{ AVX512 (on Skylake)}")
        f.write(" & \\multicolumn{4}{|c|}{ AVX512 (on KNL)} \\\\\n")

        f.write("\\cline{3-18} \\multicolumn{2}{c|}{}")
        f.write("& GNU & Clang & PGI & IBM ")
        f.write("& GNU & Clang & PGI & Intel ")
        f.write("& GNU & Clang & PGI & Intel ")
        f.write("& GNU & Clang & PGI & Intel \\\\ \\hline \\hline\n")
        # f.write("\\endhead\n")

        for cat in categories:
            f.write("\\multirow{" + str(par_num) + "}{*}{ \\parbox{2cm}{" +
                    cat.replace('_', '\\\\').title() + "}}")
            for par in parameters:
                f.write(" & " + mappars[par])
                for c in ['altivec-gcc', 'altivec-clang', 'altivec-pgi',
                          'altivec-ibm', 'avx2-gcc', 'avx2-clang',
                          'avx2-pgi', 'avx2-icc', 'avx512-gcc',
                          'avx512-clang', 'avx512-pgi', 'avx512-icc',
                          'knl-gcc', 'knl-clang', 'knl-pgi', 'knl-icc']:
                    f.write(" & " + "{:3.1f}".format(geometric_mean(
                        [v[2] for v in data[c][cat][par].values()]
                        )))
                f.write("\\\\ \\cline{2-18}\n")
            f.write("\\hline \\hline\n")
        f.write("\\end{longtable}")

        # Begin table 1 with altivec and avx2
        f.write("\\begin{longtable}{")
        f.write(("|p{2cm}"+"|c"*(17))+"|}\n")
        f.write("\\cline{3-18} \\multicolumn{2}{c|}{}")
        f.write(" & \\multicolumn{4}{|c|}{ Altivec (on Power8)}")
        f.write(" & \\multicolumn{4}{|c|}{ AVX2 (on Skylake)}")
        f.write(" & \\multicolumn{4}{|c|}{ AVX512 (on Skylake)}")
        f.write(" & \\multicolumn{4}{|c|}{ AVX512 (on KNL)} \\\\\n")

        f.write("\\cline{3-18} \\multicolumn{2}{c|}{}")
        f.write("& GNU & Clang & PGI & IBM ")
        f.write("& GNU & Clang & PGI & Intel ")
        f.write("& GNU & Clang & PGI & Intel ")
        f.write("& GNU & Clang & PGI & Intel \\\\ \\hline \\hline\n")
        # f.write("\\endhead\n")

        for cat in categories:
            for test in data['avx2-gcc'][cat][parameters[0]].keys():
                f.write("\\multirow{" + str(par_num) + "}{*}{ \\parbox{2cm}{" +
                        test.replace('_', '\\\\').title() + "}}")
                for par in parameters:
                    f.write(" & " + mappars[par])
                    for c in ['altivec-gcc', 'altivec-clang', 'altivec-pgi',
                              'altivec-ibm', 'avx2-gcc', 'avx2-clang',
                              'avx2-pgi', 'avx2-icc', 'avx512-gcc',
                              'avx512-clang', 'avx512-pgi', 'avx512-icc',
                              'knl-gcc', 'knl-clang', 'knl-pgi', 'knl-icc']:
                        f.write(" & " + "{:3.1f}".format(
                            data[c][cat][par][test][2]
                            ))
                    f.write("\\\\ \\cline{2-18}\n")
                f.write("\\hline \\hline\n")
        f.write("\\end{longtable}")
        f.write("\\end{document}")


def main():

    # Get results folder from first argument and check whether it exists
    if len(sys.argv) != 2:
        print("Expecting an argument with a path to a results foler")
        exit(-1)
    datadir = sys.argv[1]
    if not os.path.exists(datadir):
        print("Results folder does not exist")
        exit(-2)

    if os.path.exists('plots'):
        shutil.rmtree('plots')
    os.makedirs('plots')

    def nested_dict(n, type):
        from collections import defaultdict
        if n == 1:
            return defaultdict(type)
        else:
            return defaultdict(lambda: nested_dict(n-1, type))

    # Nested dictionary of {compiler, category, parameters, test} =
    #     [performance vec, performance novec, vector eff]
    data = nested_dict(4, list)

    # Nested dictionary of {microkernel, Architecture, Compiler, execution}
    # where execution is [rtvec, rtnovec, ctvec, ctnovec] = float
    microkernel_data = nested_dict(4, float)

    def getfolders(path):
        return filter(lambda x: os.path.isdir(os.path.join(path, x)),
                      os.listdir(path))

    print("Loading data...")
    for compiler in getfolders(datadir):
        print(compiler)
        compiler_path = os.path.join(datadir, compiler)
        load_microkernels(microkernel_data, compiler_path, compiler)
        for category in all_categories:  # getfolders(compiler_path):
            category_path = os.path.join(compiler_path, category)
            for parameters in all_parameters:  # getfolders(category_path):
                parameters_path = os.path.join(category_path, parameters)
                load_data(data, compiler, category, parameters,
                          parameters_path)

    print("\nData sanity check...")
    data_sanity_check(data)
    print("\nWriting summary to file...")
    os.makedirs(os.path.join('plots', 'latex_table'))
    print_summary(data)

    if True:
        print("\n- Ploting Summary VSpectrums..")
        os.makedirs(os.path.join('plots', 'extendedtsvc_summary'))
        path = os.path.join('plots', 'extendedtsvc_summary')
        plot_parameter(
            data, 'RUNTIME_INDEX',
            os.path.join(path, 'index_parameters.eps'), 'Index Parameters')
        plot_parameter(
            data, 'RUNTIME_LOOP_BOUNDS',
            os.path.join(path, 'loop_bound.eps'), 'Loop Bounds Parameters')
        plot_parameter(
            data, 'RUNTIME_CONDITIONS',
            os.path.join(path, 'conditional_parameters.eps'),
            'Conditional Parameters')
        # plot_parameter(
        #     data, 'RUNTIME_ARITHMETIC',
        #     os.path.join(path, 'arithmetic_parameters.eps'),
        #     'Arithmetic Parameters')
        plot_parameter(
            data, 'RUNTIME_ATTRIBUTES',
            os.path.join(path, 'variable_attributes.eps'),
            'Variable attributes')
        plot_parameter(
            data, 'RUNTIME_ALL',
            os.path.join(path, 'all.eps'),
            'All Parameters')

        plt.close("all")
        # exit(0)

    if False:
        print("\n- Compiler comparison")
        # os.makedirs(os.path.join('plots', 'categories_maxinfo_radars'))
        # os.makedirs(os.path.join('plots', 'categories_maxinfo_vspectrum'))
        os.makedirs(os.path.join('plots', 'categories_maxinfo_bars'))
        plot_max_info_architecture(
            data, 'compilers-avx2.eps', 'avx2',
            'AVX2 Compiler comparison')
        plot_max_info_architecture(
            data, 'compilers-avx512.eps', 'avx512',
            'AVX512 Compiler comparison')
        plot_max_info_architecture(
            data, 'compilers-knl.eps', 'knl',
            'KNL Compiler comparison')
        plot_max_info_architecture(
            data, 'compilers-altivec.eps', 'altivec',
            'Altivec Compiler comparison')

        plt.close("all")

    if False:
        print("\n- Detailed VSpectrums")
        os.makedirs(os.path.join('plots', 'extendedtsvc_detailed'))
        path = os.path.join('plots', 'extendedtsvc_detailed')
        plot_categories(
            data, 'avx2-icc', os.path.join(path, 'avx2-icc.eps'),
            title="AVX2 ICC Auto-vectorization")
        plot_categories(
            data, 'avx2-gcc', os.path.join(path, 'avx2-gcc.eps'),
            title="AVX2 GCC Auto-vectorization")
        plot_categories(
            data, 'avx2-pgi', os.path.join(path, 'avx2-pgi.eps'),
            title="AVX2 PGI Auto-vectorization")
        plot_categories(
            data, 'avx2-clang', os.path.join(path, 'avx2-clang.eps'),
            title="AVX2 Clang Auto-vectorization")
        plot_categories(
            data, 'avx512-icc', os.path.join(path, 'avx512-icc.eps'),
            title="AVX512 ICC Auto-vectorization")
        plot_categories(
            data, 'avx512-gcc', os.path.join(path, 'avx512-gcc.eps'),
            title="AVX512 GCC Auto-vectorization")
        plot_categories(
            data, 'avx512-pgi', os.path.join(path, 'avx512-pgi.eps'),
            title="AVX512 PGI Auto-vectorization")
        plot_categories(
            data, 'avx512-clang', os.path.join(path, 'avx512-clang.eps'),
            title="AVX512 Clang Auto-vectorization")
        plt.close("all")
        plot_categories(
            data, 'knl-icc', os.path.join(path, 'knl-icc.eps'),
            title="KNL ICC Auto-vectorization")
        plot_categories(
            data, 'knl-gcc', os.path.join(path, 'knl-gcc.eps'),
            title="KNL GCC Auto-vectorization")
        plot_categories(
            data, 'knl-pgi', os.path.join(path, 'knl-pgi.eps'),
            title="KNL PGI Auto-vectorization")
        plot_categories(
            data, 'knl-clang', os.path.join(path, 'knl-clang.eps'),
            title="KNL Clang Auto-vectorization")
        plot_categories(
            data, 'altivec-gcc', os.path.join(path, 'altivec-gcc.eps'),
            title="Altivec GCC Auto-vectorization")
        plot_categories(
            data, 'altivec-ibm', os.path.join(path, 'altivec-ibm.eps'),
            title="Altivec IBM Auto-vectorization")
        plot_categories(
            data, 'altivec-pgi', os.path.join(path, 'altivec-pgi.eps'),
            title="Altivec PGI Auto-vectorization")
        plot_categories(
            data, 'altivec-clang', os.path.join(path, 'altivec-clang.eps'),
            title="Altivec Clang Auto-vectorization")
        plt.close("all")

    if False:
        print("\n- MicroKernels")
        os.makedirs(os.path.join('plots', 'microkernels'))
        path = os.path.join('plots', 'microkernels')
        # plot_kernel(microkernel_data['ao'], os.path.join(path, 'ao.eps'),
        #             title="Ambient Occlusion")
        plot_kernel(microkernel_data['binomial'],
                    os.path.join(path, 'binomial.eps'),
                    title="Binomial Options")
        plot_kernel(microkernel_data['black-scholes'],
                    os.path.join(path, 'black-scholes.eps'),
                    title="Black-Scholes Options")
        plot_kernel(microkernel_data['convolution'],
                    os.path.join(path, 'convolution.eps'),
                    title="Convolution")
        plot_kernel(microkernel_data['mandelbrot'],
                    os.path.join(path, 'mandelbrot.eps'),
                    title="Mandelbrot")
        plot_kernel(microkernel_data['matrixmult'],
                    os.path.join(path, 'matrixmult.eps'),
                    title="Small Matrix Multiplications")
        plot_kernel(microkernel_data['stencil'],
                    os.path.join(path, 'stencil.eps'),
                    title="Stencil Computation")


if __name__ == "__main__":
    main()
