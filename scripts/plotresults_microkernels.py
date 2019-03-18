#!/usr/bin/env python3

import sys
import os
import shutil
from collections import defaultdict
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cmx

def add_line(ax, xpos, ypos):
    line = plt.Line2D([xpos, xpos], [ypos + .1, ypos],
                      transform=ax.transAxes, color='black')
    line.set_clip_on(False)
    ax.add_line(line)



def nested_dict(n, type):
    if n == 1:
        return defaultdict(type)
    else:
        return defaultdict(lambda: nested_dict(n-1, type))

# Returns a dictionary with {test,Architecture,Compiler,execution:value}
# where execution is [rtvec,rtnovec,ctvec,ctnovec]
def read_data(folder_name):

    data = nested_dict(4,float)
    
    for filename in filter(lambda x: x.split('-')[0] == 'output',os.listdir(folder_name)):
        compiler = filename.split('-')[1]
        architecture = filename.split('-')[2].split('.')[0]

        with open(os.path.join(folder_name,filename),'r') as f:
            print("Reding results for ", compiler, architecture)
            for line in f:
                # If it is a comment line, skip it
                if(line[0]=='#' or line[1] == '#'): continue
                #stencil RT NO-VEC cycles = [1964.311]
                test = str(line.split()[0])
                execution = str(line.split()[1]) + " " + str(line.split()[2])
                if len(line.split()) >= 6:
                    value_str = line.split()[5]
                    if value_str[0] == '[': value_str = value_str[1:-2]
                    value = float(value_str)
                    data[test][architecture][compiler][execution] = value
                else:
                    data[test][architecture][compiler][execution] = 'Error'
                    print("Error, could not find value for ", compiler, architecture, test, execution)

    return data


def plot_per_platform(data):

    platforms = set()

    for test, testdict in data.items():
        for arch, archdict in testdict.items():
            for compiler, compdict in archdict.items():
                platforms.add(arch+'-'+compiler)

    for platform in platforms:
        print('Ploting ', platform)
        arch, comp = platform.split('-')
        labels = []
        sp_rt_novec = []
        sp_rt_vec = []
        sp_ct_novec = []
        sp_ct_vec = []

        for test, testdict in data.items():
            compdict = testdict[arch][comp]
            def compute_speedup(d,execution):
                if d[execution] == 'Error': return 0.0
                else: return d['RT NO-VEC']/d[execution]

            labels.append(test)
            sp_rt_novec.append(compute_speedup(compdict,'RT NO-VEC'))
            sp_rt_vec.append(compute_speedup(compdict,'RT VEC'))
            sp_ct_novec.append(compute_speedup(compdict,'CT NO-VEC'))
            sp_ct_vec.append(compute_speedup(compdict,'CT VEC'))

        N = len(labels)

        fig = plt.figure()
        ax = fig.add_subplot(1,1,1, adjustable='box', aspect=0.7)
        ind = np.arange(N)
        width = 0.2
        bars1 = ax.bar(ind, sp_rt_novec, width, color='r')
        bars2 = ax.bar(ind + width, sp_rt_vec, width, color='b')
        bars3 = ax.bar(ind + 2 * width, sp_ct_novec, width, color='g')
        bars4 = ax.bar(ind + 3 * width, sp_ct_vec, width, color='y')

        # add some text for labels, title and axes ticks

        ax.set_ylabel('Speed-Up')
        ax.set_yscale('log')
        #ax.set_ylim([0,100])
        #ax.set_title('Macrobenchmarks on '+platform)
        ax.set_xticks(ind + width + width/2)
        ax.set_xticklabels(labels,ha='center')
        ax.xaxis.set_ticks_position('none')
        #ax.grid()

        ax.legend((bars1[0], bars2[0],bars3[0],bars4[0]), (
            'Compile Time Hidden information\nno auto-vectorization',
            'Compile Time Hidden information\nauto-vectorization',
            'Compile Time Exposed information\nno auto-vectorization',
            'Compile Time Exposed information\nauto-vectorization'),
            loc='upper left')
        fname = os.path.join(os.path.join('plots','perplatform'),platform+'.eps')
        fig.set_size_inches(12, 7)
        fig.tight_layout()
        plt.savefig(fname, format='eps')


 
def plot_per_test(data):

    plt.rc('font', size=12) 
    titlemap = { 'ao': 'Ambient Occlusion',
            'binomial' : 'Binomial Options',
            'black-scholes' : 'Black-Scholes Options',
            'convolution' : 'Convolution',
            'mandelbrot' : 'Mandelbrot',
            'matrixmult' : 'Small Matrix Multiplications',
            'stencil' : 'Stencil computation'}

    colorm = cmx.Set1.colors

    for test, testdict in data.items():
        print('Ploting ',test)
        labels = []
        sp_rt_novec = []
        sp_rt_vec = []
        sp_ct_novec = []
        sp_ct_vec = []

        for arch, archdict in testdict.items():
            for compiler, compdict in archdict.items():
                def compute_speedup(d,execution):
                    if d[execution] == 'Error': return 0.0
                    else: return d['RT NO-VEC']/d[execution]

                labels.append(arch+'-'+compiler)
                sp_rt_novec.append(compute_speedup(compdict,'RT NO-VEC'))
                sp_rt_vec.append(compute_speedup(compdict,'RT VEC'))
                sp_ct_novec.append(compute_speedup(compdict,'CT NO-VEC'))
                sp_ct_vec.append(compute_speedup(compdict,'CT VEC'))

        N = len(labels)
        
        fig = plt.figure()
        ax = fig.add_subplot(1,1,1)
        box = ax.get_position()
        ax.set_position([box.x0, box.y0 + box.height * 0.05,
                             box.width, box.height * 0.95])
        xticks = np.arange(N) + 1
        width = 0.2

        bars1 = ax.bar(xticks - 1.5 * width, sp_rt_novec, width, color=colorm[0], edgecolor='black', hatch='//', align='center')
        bars3 = ax.bar(xticks + 0.5 * width, sp_ct_novec, width, color=colorm[2],  edgecolor='black', hatch='', align='center')
        bars2 = ax.bar(xticks - 0.5 * width, sp_rt_vec, width, color=colorm[1],  edgecolor='black', hatch='-', align='center')
        bars4 = ax.bar(xticks + 1.5 * width, sp_ct_vec, width, color=colorm[3],  edgecolor='black', hatch='\\\\', align='center')
        ax.set_xticks(xticks)
        ax.set_xticklabels([ l.split('-')[1] for l in labels ])
        ax.set_xlim(0.5, N + 0.5)
        ax.yaxis.grid(True)

        # add some text for labels, title and axes ticks
        ax.set_ylabel('Speed-Up')
        #ax.set_yscale('log')
        #ax.set_title(titlemap[test])
        ax.xaxis.set_ticks_position('none')

        scale = 1. / N
        for pos in range(N + 1):
            add_line(ax, pos * scale, -.1)
        ypos = -0.2
        pos = 0
        for arch, archdict in testdict.items():
            rpos = len(archdict)
            print(arch, rpos)
            lxpos = (pos + .5 * rpos) * scale
            ax.text(lxpos, ypos, arch, ha='center', transform=ax.transAxes)
            add_line(ax, pos * scale, ypos)
            pos += rpos
        add_line(ax, pos * scale, ypos)

        #ax.legend((bars1[0], bars2[0],bars3[0],bars4[0]), (
        #    'Configuration C1: vectorization:disabled, information:withdrawn',
        #    'Configuration C2: vectorization:enabled, information:withdrawn',
        #    'Configuration C3: vectorization:disabled, information:supplied',
        #    'Configuration C4: vectorization:enabled, information:supplied'),
        #    loc='center', bbox_to_anchor=(0.5,0.5),fancybox=True,
        #    shadow=True, ncol=2)
        fname = os.path.join(os.path.join('plots','perbenchmark'),test+'.eps')
        fig.set_size_inches(8, 5)
        fig.tight_layout()
        fig.subplots_adjust(bottom=0.2)
        plt.savefig(fname, format='eps')

if len(sys.argv) != 2:
    print("Worng number of arguments. Expencting results folder name")
    exit(-1)
else:
    results_folder = sys.argv[1]

data = read_data(results_folder)


if os.path.exists('plots'):
    shutil.rmtree('plots')

os.makedirs('plots')
folder = os.path.join('plots','perbenchmark')
os.makedirs(folder)
#folder = os.path.join('plots','perplatform')
#os.makedirs(folder)

plot_per_test(data)
#plot_per_platform(data)
exit()
