#!/usr/bin/env python

import sys
import os
from collections import defaultdict
import numpy as np
import matplotlib.pyplot as plt

colors = ('r', 'c', 'm', 'y', 'k', 'b', 'g', 'r', 'c', 'm')

palette = map (lambda x: [float(x[0])/255, float(x[1])/255, float(x[2])/255], [
        [240,163,255],[0,117,220],[153,63,0],[76,0,92],
        [25,25,25],[0,92,49],[43,206,72],[255,204,153],
        [128,128,128],[148,255,181],[143,124,0],[157,204,0],
        [194,0,136],[0,51,128],[255,164,5],[255,168,187],
        [66,102,0],[255,0,16],[94,241,242],[0,153,143],
        [224,255,102],[116,10,255],[153,0,0],[255,255,128],
        [255,255,0],[255,80,5]] )


all_labels = ( "LINEAR_DEPENDENCE", "INDUCTION_VARIABLE", "GLOBAL_DATA_FLOW"
        "CONTROL_FLOW", "SYMBOLICS", "STATEMENT_REORDERING",
        "LOOP_RESTRUCTURING", "NODE_SPLITTING", "EXPANSION",
        "CROSSING_THRESHOLDS", "REDUCTIONS", "RECURRENCES",
        "SEARCHING", "PACKING", "LOOP_REROLLING", "EQUIVALENCING",
        "INDIRECT_ADDRESSING", "CONTROL_LOOPS")
all_parameters = ('original', 'None', 'RUNTIME_ARITHMETIC', 'RUNTIME_INDEX',
        'RUNTIME_CONDITIONS', 'RUNTIME_LOOP_BOUNDS', 'RUNTIME_ALL')



def add_box(ax, name, values, labels):
    for value, label, color in zip(values, labels, palette):
        ax.bar(0, 0.025, width=1, bottom=value, color=color, linewidth=0, label=label)
        #ax.text(0.5, value+0.05, label.title().replace("_"," "), ha='center', va='bottom', fontsize=10)

    mean = np.mean(filter(lambda x: x < 8, values))
    ax.axhline(mean,0,1,color='black',linestyle="--")
    ax.text(0.5, mean+0.05, "Avg. Mean", ha='center', va='bottom', fontsize=12)
    ax.set_ylim(bottom=0, top=2)
    ax.tick_params(axis='x', which='both', bottom='off', top='off', labelbottom='off')
    ax.set_xlabel(name.title().replace("_"," "), rotation=-15)


    pass

def plot_chart(charts, labels, values, outputfile, ylabel='Vector efficiency' ):

    #Find existing elements
    fig, axis = plt.subplots(1,len(charts) + 1)

    for i, ax in enumerate(axis[:-1]):
        add_box(ax, charts[i], values[i], labels) 

    #ax.plot([0., 4.5], [0.4, 3.4], "k--")

    axis[0].set_ylabel(ylabel)
    axis[-1].axis('off')
    axis[-2].legend(loc="center left", bbox_to_anchor=(1.2,0.5), fontsize = 'x-small')
    fig.suptitle('Test auto-vectorization')

    #plt.show()
    plt.savefig(outputfile)


if len(sys.argv) != 2:
    print "Expecting an argument with a path to a results foler"
    exit(-1)
if not os.path.exists(sys.argv[1]):
    print "Results folder does not exist"
    exit(-2)

datadir = sys.argv[1]
resultsdir = sys.argv[1]+"_plots"

if os.path.exists(resultsdir):
    print "Results dir", resultsdir," already exist, remove folder to redraw the plots"
    exit(-3)

#os.makedirs(resultsdir)

def getfolders(path):
    return filter(lambda x: os.path.isdir(os.path.join(path,x)),os.listdir(path))

#Nested dictionary of: compiler, category, parameters, test : [performance vec, performance novec, vector eff]
data = defaultdict(lambda : defaultdict(lambda :defaultdict(dict)))
#Dictionary test : checksum
checksum_dict = {}


def check_correctness(test, checksum):
    # if it is first time, store checksum
    if test not in checksum_dict:
        checksum_dict[test] = checksum
    else:
        if checksum_dict[test] == checksum:
            return
        else:
            print "Error checksum in ", test
            return

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
                        #check_correctness(test,cs1)
                        #check_correctness(test,cs2)
                        if cs1 != cs2:
                            print "Warning", compiler, category, parameters, test, "checksums differ!"
                        if float(novec_perf) == 0.0 or float(vec_perf) == 0.0:
                            print "Warning", compiler, category, parameters, test, "contains 0"
                        else:
                            data[compiler][category][parameters][test] = [
                                float(vec_perf),
                                float(novec_perf),
                                float(novec_perf)/float(vec_perf)
                                ]
                    else:
                        print "Warning, some lines are different!"


print "Loading data..."
for compiler in getfolders(datadir):
    print compiler
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


print "Ploting charts..."
print "- Compiler comparison"
# all compilers, all categories, original tsc
vals = []
labs = []
char = data.keys()

for cat in data[data.keys()[0]].keys(): # dict keys first to not mess with order
    labs.append(cat)
    vals2 = []
    for comp in data.keys():
        par = 'original'
        vals2.append(np.mean([x[2] for x in data[comp][cat][par].values()]))
    vals.append(vals2)

vals = map(list, zip(*vals))
labs = map(lambda x: x.title().replace("_"," "), labs)
plot_chart(char, labs , vals, 'compiler.png')

print "- Compiler comparison"
# all compilers, all categories, original tsc
vals = []
labs = []
char = data.keys()

for cat in data[data.keys()[0]].keys(): # dict keys first to not mess with order
    labs.append(cat)
    vals2 = []
    for comp in data.keys():
        par = 'original'
        gcc_vec = np.mean([x[0] for x in data['gcc'][cat][par].values()])
        vals2.append(gcc_vec / np.mean([x[0] for x in data[comp][cat][par].values()]))
    vals.append(vals2)

vals = map(list, zip(*vals))
labs = map(lambda x: x.title().replace("_"," "), labs)
plot_chart(char, labs , vals, 'compiler_speedup.png', ylabel = 'Performance against gcc')




print "- icc hidden info"
comp = 'icc'
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

vals = map(list, zip(*vals))
labs = map(lambda x: x.title().replace("_"," "), labs)

#for i,chart in enumerate(char):
#    print chart, vals[i]
#    print np.mean(vals[i])

plot_chart(char, labs , vals, 'icc.png')

print "- gcc hidden info"
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

vals = map(list, zip(*vals))
labs = map(lambda x: x.title().replace("_"," "), labs)

#for i,chart in enumerate(char):
#    print chart, vals[i]
#    print np.mean(vals[i])

plot_chart(char, labs , vals, 'gcc.png')

print "- icc perf against original"
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

vals = map(list, zip(*vals))
labs = map(lambda x: x.title().replace("_"," "), labs)

#for i,chart in enumerate(char):
#    print chart, vals[i]
#    print np.mean(vals[i])

plot_chart(char, labs , vals, 'icc_speedup.png', ylabel='Performance against original')

print "- gcc perf against original"
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

vals = map(list, zip(*vals))
labs = map(lambda x: x.title().replace("_"," "), labs)

#for i,chart in enumerate(char):
#    print chart, vals[i]
#    print np.mean(vals[i])

plot_chart(char, labs , vals, 'gcc_unsafe_speedup.png', ylabel='Performance against original')



exit(0)
"""
for comp in data.keys():
    for cat in data[comp].keys():
        for par in data[comp][cat].keys():
            print comp, cat, par, np.mean([x[2] for x in data[comp][cat][par].values()])
"""
#val = [  for x in data.keys()]
#plot_chart()


#plot_chart()
