#!/usr/bin/env python

import sys
import os
from collections import defaultdict
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import ConnectionPatch

test_sets = {
        "RUNTIME_ARITHMETIC" : [ "S000", "S1112", "S453", "S452", "S242", "S2251", "S252", \
    "S254", "S255", "S256", "S258", "S281", "S1281", "S291", "S292", "S2102", \
    "S31111", "S317", "S4112"],

        "RUNTIME_INDEX" : [ "S111", "S1111", "S112", "S113", "S1113", "S116", "S118",\
    "S119", "S1119", "S121", "S122", "S126", "S128", "S131", "S141", "S151",\
    "S431", "S161", "S275", "S277", "S171", "S172", "S173", "S174", "S175",\
    "S176", "S211", "S212", "S1213", "S221", "S1221", "S222", "S231", "S232",\
    "S233", "S2233", "S235", "S241", "S242", "S243", "S244", "S1244", "S2244",\
    "S3251", "S256", "S257", "S258", "S261", "S281", "S291", "S292", "S293",\
    "S2111", "S321", "S322", "S323", "S351", "S352", "S353", "S421", "S422",\
    "S423", "S424", "S4114", "S4116", "S4117"],

        "RUNTIME_CONDITIONS" : [ "S123", "S124", "S161", "S1161", "S162", "S271", "S272",\
    "S273", "S274", "S275", "S276", "S277", "S278", "S279", "S1279", "S2710",\
    "S2711", "S441", "S443", "S481", "S258", "S314", "S315", "S316", "S3111",\
    "S331", "S341", "S342", "S343"]
}

def rgb(x):
    return [float(x[0])/255, float(x[1])/255, float(x[2])/255]

palette = {
        "Bdw-Icc" : rgb([240,163,255]),
        "Bdw-Gcc": rgb([0,117,220]),
        "Bdw-Pgi": rgb([153,63,0]),
        "Xeon-Clang": rgb([76,0,92]),
        "Pwr8-Xlc": rgb([25,25,25]),
        "Pwr8-Gcc": rgb([0,92,49]),
        "Pwr8-Pgi": rgb([43,206,72]),
        "Knl-Gcc": rgb([255,204,153]),
        "Knl-Icc": rgb([255,204,153]),
        "Knl-Clang": rgb([128,128,128]),
        "Linear Dependence" : rgb([240,163,255]),
        "Induction Variable": rgb([0,117,220]),
        "Global Data Flow": rgb([153,63,0]),
        "Control Flow": rgb([76,0,92]),
        "Symbolics": rgb([25,25,25]),
        "Statement Reordering": rgb([0,92,49]),
        "Loop Restructuring": rgb([43,206,72]),
        "Node Splitting": rgb([255,204,153]),
        "Expansion": rgb([128,128,128]),
        "Crossing Thresholds" : rgb([240,163,255]),
        "Reductions": rgb([0,117,220]),
        "Recurrences": rgb([153,63,0]),
        "Searching": rgb([76,0,92]),
        "Packing": rgb([25,25,25]),
        "Loop Rerolling": rgb([0,92,49]),
        "Equivalencing": rgb([43,206,72]),
        "Indirect Addressing": rgb([255,204,153]),
        "Control Loops": rgb([128,128,128]),
    }

categories = [
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


def add_box(ax, name, values, labels, draw_mean=False):

    if len(list(values)) != len(list(labels)):
        print("Error: Inconsisten number of values/labels")
        exit(-1)
    
    for value, label in zip(values, labels):
        if not np.isnan(value):
            ax.axhline(value, 0, 1, color=palette[label], label=label)

    if draw_mean:
        mean = np.mean([v for v in values if not np.isnan(v)])
        ax.axhline(mean,0,1,color='black',linestyle="--")
        ax.text(0.5, mean+0.05, "Avg. Mean = " + "{:.2f}".format(mean), ha='center', va='bottom', fontsize=8)
    ax.set_ylim(bottom=0, top=4)
    ax.tick_params(axis='x', which='both', bottom='off', top='off', labelbottom='off')
    ax.set_xlabel(name.title().replace("_","\n"), rotation=0)

def plot_chart(charts, labels, values, outputfile, title= "Auto-vectorization",
        ylabel='Vector efficiency', connect=False, draw_mean=False ):

    if len(list(values)) != len(list(charts)):
        print("Error: Inconsistent number of charts/values")
        exit(-1)

    #Find existing elements
    fig, axis = plt.subplots(1,len(charts) + 1)

    for ax in axis:
        ax.set_facecolor('none')

    for ax, c, v in zip(axis[:-1],list(charts),list(values)):
        add_box(ax, c, v, labels, draw_mean) 

    for idx, (val,lab) in enumerate(sorted(zip(values[-1],labels))):
        v_loc = idx*(float(1)/len(labels))
        axis[-1].text(0.5,v_loc,lab, ha='left', va='center', fontsize=10)
        if connect:
            xy = (1,val)
            xy2 = (0.45,v_loc)
            con = ConnectionPatch(xyA=xy, xyB=xy2, coordsA="data",coordsB="data", axesA=axis[-2], axesB=axis[-1], color=palette[lab], connectionstyle="arc,angleA=-180,angleB=-180,armA=-20,armB=20,rad=0")
            axis[-2].add_artist(con)
        else:
            con = ConnectionPatch( xyA=(0.25,v_loc), xyB=(0.45,v_loc),
                           coordsA="data", coordsB="data",
                           axesA=axis[-1], axesB=axis[-1],
                           color=palette[lab])
            axis[-1].add_artist(con)

    

    for ax in axis:
        ax.set_facecolor('none')

   
    if len(values) > 1 and connect:
        #Connect same labels among inner charts (just 0 to 1 implemented)
        for val1, val2, lab in zip(values[0],values[1],labels):
            con = ConnectionPatch( xyA=(1,val1), xyB=(0,val2),
                           coordsA="data", coordsB="data",
                           axesA=axis[0], axesB=axis[1],
                           color=palette[lab])
            axis[0].add_artist(con)
    
    

    # Remove all labels but leftmost
    axis[0].set_ylabel(ylabel)
    for ax in axis[1:-1]:
        ax.set_yticklabels([])

    # Remove rightmost box to place the legend
    axis[-1].axis('off')

    # We need to draw the canvas, otherwise the labels won't be positioned and 
    # won't have values yet.
    fig.canvas.draw()

    labels = axis[0].get_yticks().tolist()
    #print(labels)
    labels[-1] = '(AVX512 vector length) ' + str(labels[-1])
    #print(labels)
    axis[0].set_xticklabels(labels)

    fig.suptitle(title)
    fig.set_size_inches(4,4)
    plt.savefig(outputfile, dpi=100)


def getfolders(path):
    return filter(lambda x: os.path.isdir(os.path.join(path,x)),os.listdir(path))

def load_data(data, compiler, category, parameters, parameters_path):

    # Set filenames for tsc or tsc_runtime results
    if parameters == 'original':
        vecfname = 'runvec.txt'
        novecfname = 'runnovec.txt'
    else:
        vecfname = 'runrtvec.txt'
        novecfname = 'runrtnovec.txt'

    # Open results files and entries to 'data'
    with open(os.path.join(parameters_path,vecfname)) as vecf:
        with open(os.path.join(parameters_path,novecfname)) as novecf:
            for linevec, linenovec in zip(vecf.readlines(),novecf.readlines()):
                if linevec[0] == 'S':
                    if linenovec.split()[0] == linevec.split()[0]:
                        test, novec_perf, cs1 = linenovec.split()
                        test, vec_perf, cs2 = linevec.split()
                        if abs(float(cs1) - float(cs2)) > abs(float(cs1)*0.01):
                            print("Warning checksums differ! ", compiler,
                                    category, parameters, test, cs1," ",cs2)
                        elif float(novec_perf) == 0.0 or float(vec_perf) == 0.0:
                            print("Warning contains 0 " , compiler,
                                    category, parameters, test)
                        elif float(novec_perf)/float(vec_perf) > 16.0:
                            print("Warning outlier " , compiler, category,
                                parameters, test, float(novec_perf),
                                float(vec_perf), float(novec_perf)/float(vec_perf))
                        else:
                            data[compiler][category][parameters][test] = [
                                float(vec_perf),
                                float(novec_perf),
                                float(novec_perf)/float(vec_perf)
                                ]
                    else:
                        print("Warning, some lines are different!")

def all_categories(data):
    categories = set()
    for compiler in data.keys():
        for cat in data[compiler].keys():
            categories.add(cat)
    return list(categories)


def plot_compilers(data, output, architecture, title="", speedup_vs=None):
    # all compilers, all categories, original tsc
    vals = []
    labs = []
    
    compiler_list = [ x for x in data.keys() if architecture in x]
    
    for cat in all_categories(data): 
        labs.append(cat)
        vals2 = []
        for comp in compiler_list :
            par = 'None'

            if speedup_vs != None:
                baseline = np.mean([x[0] for x in data[speedup_vs][cat][par].values()])
                vals2.append(baseline / np.mean([x[0] for x in data[comp][cat][par].values()]))
            else:
                vals2.append(np.mean([x[2] for x in data[comp][cat][par].values()]))
        vals.append(vals2)

    vals = [list(i) for i in zip(*vals)]
    labs = [x.title().replace("_"," ") for x in labs]
    char = [x[4:]  for x in compiler_list]

    #print("charts",char)
    #print("LABS", labs)
    #print("vals", vals)
    plot_chart(char, labs , vals, output, title=title, ylabel = "Vector Efficiency", connect=False, draw_mean=True)

def plot_categories(data, comp, output, title="", speedup=False):
    labs = []
    vals = []
    char = ('None', 'RUNTIME_INDEX',
            'RUNTIME_CONDITIONS', 'RUNTIME_LOOP_BOUNDS', 'RUNTIME_ALL')

    for cat in data[comp].keys():
        labs.append(cat)
        vals2 = []
        for par in char:
            if speedup:
                baseline = np.mean([x[0] for x in data[comp][cat]['None'].values()])
                vals2.append( baseline / np.mean([x[0] for x in data[comp][cat][par].values()]) )
            else:
                vals2.append(np.mean([x[2] for x in data[comp][cat][par].values()]))
        vals.append(vals2)

    vals = [list(i) for i in zip(*vals)]
    labs = [x.title().replace("_"," ") for x in labs]

    plot_chart(char, labs , vals, output, title=title, ylabel = "Vector Efficiency", connect=False, draw_mean=True)


def plot_new(data, detailed_summary, parameter, output, title="", speedup=False):
    labs = []
    vals_ct = []
    vals_rt = []
    char = ('Compile Time \n Known', 'Compile Time \n Hidden')
    
    for compiler in data.keys():
        labs.append(compiler)
        none_sum = 0
        none_count = 0
        par_sum = 0
        par_count = 0
        for category in data[compiler].keys():
            # average of none and average of parameter (of should I compute each test and then avg diff?)
            cat_none_sum = 0
            cat_none_count = 0
            for test, value in data[compiler][category]['None'].items():
                if (parameter not in test_sets) or (test in test_sets[parameter]): 
                    cat_none_sum = none_sum + value[2]
                    cat_none_count = none_count + 1

            if(cat_none_count==0):
                detailed_summary[compiler]['None'][category] = 0
            else:
                detailed_summary[compiler]['None'][category] = float(cat_none_sum/cat_none_count)

            none_sum = none_sum + cat_none_sum
            none_count = none_count + cat_none_count

            cat_par_sum = 0
            cat_par_count = 0
            for test, value in data[compiler][category][parameter].items():
                if (parameter not in test_sets) or (test in test_sets[parameter]): 
                    cat_par_sum = par_sum + value[2]
                    cat_par_count = par_count + 1
            if(cat_par_count==0):
                detailed_summary[compiler][parameter][category] = 0
            else:
                detailed_summary[compiler][parameter][category] = float(cat_par_sum/cat_par_count)

            par_sum = par_sum + cat_par_sum
            par_count = par_count + cat_par_count

        if (none_count != par_count):
            print("Warrning!: " + str(compiler)+ " mismatching number of tests. Set to 0")
        if(none_count==0):
            print("Warrning!: " + str(compiler)+ " NONE contains no tests. Set to 0")
            vals_ct.append(0)
        else:
            vals_ct.append(none_sum/none_count)

        if(par_count==0):
            print("Warrning!: " + str(compiler)+ " " + str(parameter) + " contains no tests. Set to 0")
            vals_rt.append(0)
        else:
            vals_rt.append(par_sum/par_count)

    vals = [vals_ct,vals_rt]
       
    labs = [x.title() for x in labs]
    plot_chart(char, labs , vals, output, title=title, ylabel = "Vector Efficiency", connect=True)


def print_summary(detailed_summary,categories):
    # Print latex table
    #
    # \begin{tabular}{ l | c | r }
    #   \hline          
    #     1 & 2 & 3 \\
    #     4 & 5 & 6 \\
    #     7 & 8 & 9 \\
    #   \hline  
    # \end{tabular}

    rotation = 90
    cat_num = len(categories)
    par_num = len(detailed_summary[next(iter(detailed_summary.keys()))])

    with open('output.tex','w') as f:
        #f.write("\\begin{table}[hpt] \n")
        #f.write("\\centering \n")
        f.write("\\begin{adjustbox}{center} \n")
        f.write("\\begin{longtable}{|c|c" + ("|c"*(cat_num-5)) +"|}\n")
        f.write("\\hline\n")
        f.write("\\begin{turn}{"+str(rotation)+"}Architecture-Compiler\\end{turn} & ")
        #f.write("\\begin{turn}{"+str(rotation)+"}Compiler\\end{turn} & ")
        f.write("\\begin{turn}{"+str(rotation)+"}Parameters\\end{turn} ")
        for cat in categories[:-5]:
            f.write(" & \\begin{turn}{"+str(rotation)+"}"+cat.title().replace("_"," ") +"\\end{turn} ")

        f.write("\\\\ \n")

        for archcomp in sorted(detailed_summary.keys()):
            arch, comp = archcomp.split("-")
            #print(arch)
            #print(comp)
            f.write("\\hline \n")
            f.write("  \multirow{"+str(par_num)+"}{*}{"+archcomp+"} & P "+ ("& 1.0 "*(cat_num-5))  +" \\\\ \n")
            for i in range(par_num-1):
                f.write(" & P "+ ("& 1.0 "*(cat_num-5))  +" \\\\ \n")
        f.write("\\hline \n")
        f.write("\\end{longtable}\n")
        f.write("\\end{adjustbox}\n")


        #f.write("{}")
        #f.write("Header: "+ str(categories))
        #for compiler in detailed_summary.keys():
        #    f.write(compiler)
        #    for parameter in detailed_summary[compiler].keys():
        #        values = [str(x) + " " +str(detailed_summary[compiler][parameter][x]) for x in categories ]
        #        f.write(parameter + " : " + str(values))
 
def main():

    # Get argv 1 and error checking
    if len(sys.argv) != 2:
        print("Expecting an argument with a path to a results foler")
        exit(-1)
    datadir = sys.argv[1]
    if not os.path.exists(datadir):
        print("Results folder does not exist")
        exit(-2)

    #Nested dictionary of: compiler, category, parameters, test : [performance vec, performance novec, vector eff]
    data = defaultdict(lambda : defaultdict(lambda :defaultdict(dict)))
    #Nested dictionary of: compiler, parameter, category : avg_vector_eff
    detailed_summary = defaultdict(lambda : defaultdict(lambda :defaultdict(float)))

    print("Loading data...")
    for compiler in getfolders(datadir):
        print(compiler)
        compiler_path = os.path.join(datadir,compiler)
        for category in getfolders(compiler_path):
            category_path = os.path.join(compiler_path,category)
            for parameters in getfolders(category_path):
                parameters_path = os.path.join(category_path,parameters)
                load_data(data, compiler,category,parameters,parameters_path)

    print("")

    print("Ploting Parameters charts...")
    plot_new(data, detailed_summary, 'RUNTIME_INDEX','rindex.png', 'Index Parameters')
    plot_new(data, detailed_summary, 'RUNTIME_LOOP_BOUNDS','rbound.png', 'Loop Bounds Parameters')
    plot_new(data, detailed_summary, 'RUNTIME_CONDITIONS','rcond.png', 'Conditional Parameters')
    plot_new(data, detailed_summary, 'RUNTIME_ARITHMETIC','rarith.png', 'Arithmetic Parameters')
    plot_new(data, detailed_summary, 'RUNTIME_ALL','rall.png', 'All Parameters')
    print_summary(detailed_summary, all_categories(data))

    exit(0)
    print("- Compiler comparison")
    plot_compilers(data, 'compilers-bdw.png', 'bdw', 'Broadwell Compiler comparison')
    plot_compilers(data, 'compilers-pwr8.png', 'pwr8', 'Power8 Compiler comparison')
    plot_compilers(data, 'compilers_bdw_vs_gcc.png', 'bdw', 'Broadwell Performance against gcc', speedup_vs='bdw-gcc')
    plot_compilers(data, 'compilers_pwr8_vs_gcc.png', 'pwr8', 'Power8 Performance against gcc', speedup_vs='pwr8-gcc')

    print("- Categories comparison")
    plot_categories(data, 'bdw-icc', 'bdw-icc.png', title="BDW ICC Auto-vectorization", speedup=False)
    plot_categories(data, 'bdw-gcc', 'bdw-gcc.png', title="BDW GCC Auto-vectorization", speedup=False)
    plot_categories(data, 'bdw-pgi', 'bdw-pgi.png', title="BDW PGI Auto-vectorization", speedup=False)
    plot_categories(data, 'pwr8-gcc', 'pwr8-gcc.png', title="Power8 GCC Auto-vectorization", speedup=False)
    plot_categories(data, 'pwr8-xlc', 'pwr8-xlc.png', title="Power8 XLC Auto-vectorization", speedup=False)
    plot_categories(data, 'knl-icc', 'knl-icc.png', title="KNL ICC Auto-vectorization", speedup=False)
    #plot_categories(data, 'pwr8-pgi', 'pwr8-pgi.png', title="Power8 PGI Auto-vectorization", speedup=False)
    #plot_categories(data, 'pwr8-clang', 'pwr8-clang.png', title="Power8 Clang Auto-vectorization", speedup=False)
    exit(0)
    plot_categories(data, 'icc_unsafe', 'icc_speedup.png', title="ICC performance angainst original", speedup=True)
    plot_categories(data, 'gcc_unsafe', 'gcc.png', title="GCC Auto-vectorization", speedup=False)
    plot_categories(data, 'gcc_unsafe', 'gcc_speedup.png', title="GCC performance angainst original", speedup=True)
    plot_categories(data, 'clang', 'clang.png', title="Clang Auto-vectorization", speedup=False)
    plot_categories(data, 'clang', 'clang_speedup.png', title="Clang performance angainst original", speedup=True)

    # TODO: also add ISA comparsion and architecture comparison

if __name__ == "__main__":
    main()
