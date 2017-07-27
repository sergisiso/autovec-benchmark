#!/usr/bin/env python

import sys
import os
from collections import defaultdict
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import ConnectionPatch


def rgb(x):
    return [float(x[0])/255, float(x[1])/255, float(x[2])/255]

palette = {
        "Control Flow" : rgb([240,163,255]),
        "Crossing Thresholds": rgb([0,117,220]),
        "Expansion": rgb([153,63,0]),
        "Global Data Flow": rgb([76,0,92]),
        "Induction Variable": rgb([25,25,25]),
        "Linear Dependence": rgb([0,92,49]),
        "Loop Restructuring": rgb([43,206,72]),
        "Node Splitting": rgb([255,204,153]),
        "Recurrences": rgb([128,128,128]),
        "Reductions": rgb([148,255,181]),
        "Statement Reordering": rgb([143,124,0]),
        "Symbolics": rgb([157,204,0]),
        "Searching": rgb([194,0,136]),
        "Recurrences": rgb([0,51,128]),
        "Equivalencing": rgb([255,164,5]),
        "Packing": rgb([255,168,187]),
        "Indirect Addressing": rgb([66,102,0]),
        "Loop Rerolling": rgb([255,0,16]),
        "": rgb([94,241,242]),
        "": rgb([0,152,143]),
        "": rgb([224,255,102]),
        "": rgb([116,10,255]),
        "": rgb([153,0,0]),
        "": rgb([255,255,128]),
        "": rgb([255,255,0]),
        "": rgb([255,80,5]),
    }


def add_box(ax, name, values, labels):

    if len(list(values)) != len(list(labels)):
        print("Error: Inconsisten number of values/labels")
        exit(-1)
    
    for value, label in zip(values, labels):
        if not np.isnan(value):
            ax.axhline(value, 0, 1, color=palette[label], label=label)

    mean = np.mean([v for v in values if not np.isnan(v)])
    ax.axhline(mean,0,1,color='black',linestyle="--")
    ax.text(0.5, mean+0.05, "Avg. Mean = " + "{:.2f}".format(mean), ha='center', va='bottom', fontsize=10)
    ax.set_ylim(bottom=0, top=16)
    ax.tick_params(axis='x', which='both', bottom='off', top='off', labelbottom='off')
    ax.set_xlabel(name.title().replace("_","\n"), rotation=-15)

def plot_chart(charts, labels, values, outputfile, ylabel='Vector efficiency' ):

    if len(list(values)) != len(list(charts)):
        print("Error: Inconsistent number of charts/values")
        exit(-1)

    #Find existing elements
    fig, axis = plt.subplots(1,len(charts) + 1)

    for ax in axis:
        ax.set_facecolor('none')

    for ax, c, v in zip(axis[:-1],list(charts),list(values)):
        add_box(ax, c, v, labels) 

    for idx, val in enumerate(labels):
        axis[-1].text(0.3,idx*(1/len(labels)),val, ha='left', va='bottom', fontsize=10)
    
    for ax in axis:
        ax.set_facecolor('none')

   

    xy = (0.5,12)
    con = ConnectionPatch(xyA=xy, xyB=xy, coordsA="data",coordsB="data", axesA=axis[1], axesB=axis[2], color="red")
    axis[1].add_artist(con)
    xy = (0.5,11)
    con = ConnectionPatch(xyA=xy, xyB=xy, coordsA="data",coordsB="data", axesA=axis[0], axesB=axis[1], color="red")
    axis[0].add_artist(con)
    
    xy = (1,6)
    xy2 = (0.1,0.25)
    con = ConnectionPatch(xyA=xy, xyB=xy2, coordsA="data",coordsB="data", axesA=axis[2], axesB=axis[3], color="red", connectionstyle="arc,angleA=0,angleB=-180,armA=90,armB=45,rad=0")
    #con = ConnectionPatch(xyA=xy, xyB=xy, coordsA="data",coordsB="data", axesA=axis[2], axesB=axis[3], color="red", connectionstyle="bar,angle=180,fraction=-0.2")
    axis[2].add_artist(con)
    

        #for ax, ax2, v, v2 in zip():

    # Remove all labels but leftmost
    axis[0].set_ylabel(ylabel)
    for ax in axis[1:-1]:
        ax.set_yticklabels([])

    # Remove rightmost box to place the legend
    axis[-1].axis('off')

    fig.suptitle('Test auto-vectorization')
    fig.set_size_inches(9,6)
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


def plot_compilers(data, output, title="", speedup_vs=None):
    # all compilers, all categories, original tsc
    vals = []
    labs = []
    for cat in all_categories(data): 
        labs.append(cat)
        vals2 = []
        for comp in data.keys():
            par = 'original'
            if speedup_vs != None:
                baseline = np.mean([x[0] for x in data[speedup_vs][cat][par].values()])
                vals2.append(baseline / np.mean([x[0] for x in data[comp][cat][par].values()]))
            else:
                vals2.append(np.mean([x[2] for x in data[comp][cat][par].values()]))
        vals.append(vals2)

    vals = [list(i) for i in zip(*vals)]
    labs = [x.title().replace("_"," ") for x in labs]
    char = [x.split("_")[0] for x in data.keys()]
    plot_chart(char, labs , vals, output, ylabel = title)

def plot_categories(data, comp, output, title="", speedup=False):
    labs = []
    vals = []
    char = ('original', 'RUNTIME_INDEX',
            'RUNTIME_CONDITIONS', 'RUNTIME_LOOP_BOUNDS', 'RUNTIME_ALL')

    for cat in data[comp].keys():
        labs.append(cat)
        vals2 = []
        for par in char:
            if speedup:
                baseline = np.mean([x[0] for x in data[comp][cat]['original'].values()])
                vals2.append( baseline / np.mean([x[0] for x in data[comp][cat][par].values()]) )
            else:
                vals2.append(np.mean([x[2] for x in data[comp][cat][par].values()]))
        vals.append(vals2)

    vals = [list(i) for i in zip(*vals)]
    labs = [x.title().replace("_"," ") for x in labs]
    plot_chart(char, labs , vals, output, ylabel = title)


 
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

    print("Loading data...")
    for compiler in getfolders(datadir):
        print(compiler)
        compiler_path = os.path.join(datadir,compiler)
        for category in getfolders(compiler_path):
            category_path = os.path.join(compiler_path,category)
            load_data(data, compiler,category,'original',category_path)
            for parameters in getfolders(category_path):
                parameters_path = os.path.join(category_path,parameters)
                load_data(data, compiler,category,parameters,parameters_path)


    #print "Compilers: ", len(data.keys())
    #for com in data.keys():
    #    print "  Categories: ", len(data[com].keys())
    #    for cat in data[com].keys():
    #        print "    Parameters: ", len(data[com][cat].keys())
    #        for par in data[com][cat].keys():
    #            print "      Tests:", len(data[com][cat][par].keys())


    print("Ploting charts...")

    print("- Compiler comparison")
    plot_compilers(data, 'compilers.png', 'Compiler comparison')
    exit()
    plot_compilers(data, 'compilers_vs_gcc.png', 'Performance against gcc', speedup_vs='gcc_unsafe')

    print("- Categories comparison")
    plot_categories(data, 'icc_unsafe', 'icc.png', title="ICC Auto-vectorization", speedup=False)
    plot_categories(data, 'icc_unsafe', 'icc_speedup.png', title="ICC performance angainst original", speedup=True)
    plot_categories(data, 'gcc_unsafe', 'gcc.png', title="GCC Auto-vectorization", speedup=False)
    plot_categories(data, 'gcc_unsafe', 'gcc_speedup.png', title="GCC performance angainst original", speedup=True)
    plot_categories(data, 'clang', 'clang.png', title="Clang Auto-vectorization", speedup=False)
    plot_categories(data, 'clang', 'clang_speedup.png', title="Clang performance angainst original", speedup=True)

    # TODO: also add ISA comparsion and architecture comparison

if __name__ == "__main__":
    main()
