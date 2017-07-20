#!/usr/bin/env python

import sys
import os
from collections import defaultdict
import numpy as np
import matplotlib.pyplot as plt



palette = [ [float(x[0])/255, float(x[1])/255, float(x[2])/255] for x in [
        [240,163,255],[0,117,220],[153,63,0],[76,0,92],
        [25,25,25],[0,92,49],[43,206,72],[255,204,153],
        [128,128,128],[148,255,181],[143,124,0],[157,204,0],
        [194,0,136],[0,51,128],[255,164,5],[255,168,187],
        [66,102,0],[255,0,16],[94,241,242],[0,153,143],
        [224,255,102],[116,10,255],[153,0,0],[255,255,128],
        [255,255,0],[255,80,5]] ]


all_labels = ( "LINEAR_DEPENDENCE", "INDUCTION_VARIABLE", "GLOBAL_DATA_FLOW"
        "CONTROL_FLOW", "SYMBOLICS", "STATEMENT_REORDERING",
        "LOOP_RESTRUCTURING", "NODE_SPLITTING", "EXPANSION",
        "CROSSING_THRESHOLDS", "REDUCTIONS", "RECURRENCES",
        "SEARCHING", "PACKING", "LOOP_REROLLING", "EQUIVALENCING",
        "INDIRECT_ADDRESSING", "CONTROL_LOOPS")
all_parameters = ('original', 'None', 'RUNTIME_ARITHMETIC', 'RUNTIME_INDEX',
        'RUNTIME_CONDITIONS', 'RUNTIME_LOOP_BOUNDS', 'RUNTIME_ALL')



def add_box(ax, name, values, labels):
    if len(list(values)) != len(list(labels)):
        print("Error: Inconsisten number of values/labels")
        exit(-1)
    
    for value, label, color in zip(values, labels, palette):
        #print(value,label,color)
        if not np.isnan(value):
            ax.bar( 0, 0.05, width=1, bottom=value,
                    color=color, linewidth=0, label=label)

    mean = np.mean([v for v in values if not np.isnan(v)])
    ax.axhline(mean,0,1,color='black',linestyle="--")
    #ax.text(0.5, mean+0.05, "Avg. Mean", ha='center', va='bottom', fontsize=12)
    ax.set_ylim(bottom=0, top=16)
    ax.tick_params(axis='x', which='both', bottom='off', top='off', labelbottom='off')
    ax.set_xlabel(name.title().replace("_","\n"), rotation=-15)

def plot_chart(charts, labels, values, outputfile, ylabel='Vector efficiency' ):

    if len(list(values)) != len(list(charts)):
        print("Error: Inconsistent number of charts/values")
        exit(-1)

    #Find existing elements
    fig, axis = plt.subplots(1,len(charts) + 1)

    for ax, c, v in zip(axis[:-1],list(charts),list(values)):
        add_box(ax, c, v, labels) 

    for ax in axis[1:-1]:
        ax.set_yticklabels([])

    axis[0].set_ylabel(ylabel)
    axis[-1].axis('off')
    axis[-2].legend(loc="center left", bbox_to_anchor=(1.2,0.5), fontsize = 'small')
    #fig.suptitle('Test auto-vectorization')
    fig.set_size_inches(9,6)

    #plt.show()
    plt.savefig(outputfile, dpi=100)


if len(sys.argv) != 2:
    print("Expecting an argument with a path to a results foler")
    exit(-1)
if not os.path.exists(sys.argv[1]):
    print("Results folder does not exist")
    exit(-2)

datadir = sys.argv[1]
resultsdir = sys.argv[1]+"_plots"

if os.path.exists(resultsdir):
    print("Results dir", resultsdir," already exist, remove folder to redraw the plots")
    exit(-3)

#os.makedirs(resultsdir)

def getfolders(path):
    return filter(lambda x: os.path.isdir(os.path.join(path,x)),os.listdir(path))

#Nested dictionary of: compiler, category, parameters, test : [performance vec, performance novec, vector eff]
data = defaultdict(lambda : defaultdict(lambda :defaultdict(dict)))
#Dictionary test : checksum
checksum_dict = {}

def load_data(compiler,category,parameters,parameters_path):

    # Set filenames for tsc or tsc_runtime results
    if parameters == 'original':
        vecfname = 'runvec.txt'
        novecfname = 'runnovec.txt'
    else:
        vecfname = 'runrtvec.txt'
        novecfname = 'runrtnovec.txt'

    # Open results files
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
                            print("Warning outlier " , compiler, category, parameters, test, float(novec_perf), float(vec_perf), float(novec_perf)/float(vec_perf)  )
                        else:
                            data[compiler][category][parameters][test] = [
                                float(vec_perf),
                                float(novec_perf),
                                float(novec_perf)/float(vec_perf)
                                ]
                    else:
                        print("Warning, some lines are different!")


print("Loading data...")
for compiler in getfolders(datadir):
    print(compiler)
    compiler_path = os.path.join(datadir,compiler)
    for category in getfolders(compiler_path):
        category_path = os.path.join(compiler_path,category)
        load_data(compiler,category,'original',category_path)
        for parameters in getfolders(category_path):
            parameters_path = os.path.join(category_path,parameters)
            load_data(compiler,category,parameters,parameters_path)


#print "Compilers: ", len(data.keys())
#for com in data.keys():
#    print "  Categories: ", len(data[com].keys())
#    for cat in data[com].keys():
#        print "    Parameters: ", len(data[com][cat].keys())
#        for par in data[com][cat].keys():
#            print "      Tests:", len(data[com][cat][par].keys())


print("Ploting charts...")
print("- Compiler comparison")
# all compilers, all categories, original tsc
vals = []
labs = []

def all_categories():
    categories = set()
    for compiler in data.keys():
        for cat in data[compiler].keys():
            categories.add(cat)
    return list(categories)

for cat in all_categories(): 
    labs.append(cat)
    vals2 = []
    for comp in data.keys():
        par = 'original'
        vals2.append(np.mean([x[2] for x in data[comp][cat][par].values()]))
    vals.append(vals2)

vals = [list(i) for i in zip(*vals)]
labs = [x.title().replace("_"," ") for x in labs]
char = [x.split("_")[0] for x in data.keys()]
plot_chart(char, labs , vals, 'compiler.png')

# all compilers, all categories, original tsc
vals = []
labs = []

for cat in all_categories():
    labs.append(cat)
    vals2 = []
    for comp in data.keys():
        par = 'original'
        gcc_vec = np.mean([x[0] for x in data['gcc_unsafe'][cat][par].values()])
        vals2.append(gcc_vec / np.mean([x[0] for x in data[comp][cat][par].values()]))
    vals.append(vals2)

vals = [list(i) for i in zip(*vals)]
labs = [x.title().replace("_"," ") for x in labs]
char = [x.split("_")[0] for x in data.keys()]
plot_chart(char, labs , vals, 'compiler_speedup.png', ylabel = 'Performance against gcc')

print("- icc hidden info")
comp = 'icc_unsafe'
labs = []
vals = []
char = ('original', 'RUNTIME_INDEX',
        'RUNTIME_CONDITIONS', 'RUNTIME_LOOP_BOUNDS', 'RUNTIME_ALL')

for cat in data[comp].keys():
    labs.append(cat)
    vals2 = []
    for par in char:
        vals2.append(np.mean([x[2] for x in data[comp][cat][par].values()]))
    vals.append(vals2)

vals = [list(i) for i in zip(*vals)]
labs = [x.title().replace("_"," ") for x in labs]

#for i,chart in enumerate(char):
#    print chart, vals[i]
#    print np.mean(vals[i])

plot_chart(char, labs , vals, 'icc.png')

print("- gcc hidden info")
comp = 'gcc_unsafe'
labs = []
vals = []
char = ('original', 'RUNTIME_INDEX',
        'RUNTIME_CONDITIONS', 'RUNTIME_LOOP_BOUNDS', 'RUNTIME_ALL')

for cat in data[comp].keys():
    labs.append(cat)
    vals2 = []
    for par in char:
        vals2.append(np.mean([x[2] for x in data[comp][cat][par].values()]))
    vals.append(vals2)



vals = [list(i) for i in zip(*vals)]
labs = [x.title().replace("_"," ") for x in labs]


#for i,chart in enumerate(char):
#    print chart, vals[i]
#    print np.mean(vals[i])

plot_chart(char, labs , vals, 'gcc.png')

exit(0)
print("- icc perf against original")
comp = 'icc'
labs = []
vals = []
char = ('original', 'RUNTIME_INDEX',
        'RUNTIME_CONDITIONS', 'RUNTIME_LOOP_BOUNDS', 'RUNTIME_ALL')

for cat in data[comp].keys():
    labs.append(cat)
    vals2 = []
    for par in char:
        original_vec = np.mean([x[0] for x in data[comp][cat]['original'].values()])
        vals2.append( original_vec / np.mean([x[0] for x in data[comp][cat][par].values()]) )
    vals.append(vals2)

vals = [list(i) for i in zip(*vals)]
labs = [x.title().replace("_"," ") for x in labs]

#for i,chart in enumerate(char):
#    print chart, vals[i]
#    print np.mean(vals[i])

plot_chart(char, labs , vals, 'icc_speedup.png', ylabel='Performance against original')

print("- gcc perf against original")
comp = 'gcc_unsafe'
labs = []
vals = []
char = ('original', 'RUNTIME_INDEX',
        'RUNTIME_CONDITIONS', 'RUNTIME_LOOP_BOUNDS', 'RUNTIME_ALL')

for cat in data[comp].keys():
    labs.append(cat)
    vals2 = []
    for par in char:
        original_vec = np.mean([x[0] for x in data[comp][cat]['original'].values()])
        vals2.append( original_vec / np.mean([x[0] for x in data[comp][cat][par].values()]) )
    vals.append(vals2)

vals = [list(i) for i in zip(*vals)]
labs = [x.title().replace("_"," ") for x in labs]

#for i,chart in enumerate(char):
#    print chart, vals[i]
#    print np.mean(vals[i])

plot_chart(char, labs , vals, 'gcc_unsafe_speedup.png', ylabel='Performance against original')


