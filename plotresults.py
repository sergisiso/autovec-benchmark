#!/usr/bin/env python3

import sys
import os
import shutil
import math
import statistics
from collections import defaultdict
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import ConnectionPatch
from matplotlib.path import Path
from matplotlib.spines import Spine
from matplotlib.lines import Line2D
import matplotlib.cm as cmx
import matplotlib.colors as colors
from matplotlib.projections.polar import PolarAxes
from matplotlib.projections import register_projection

remove_tests = ['S317','S2712', 'S1251']
remove_categories = [] #['REDUCTIONS','CONTROL_FLOW','SEARCHING']

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
        "Avx2-Icc" : rgb([67,0,167]),
        "Avx2-Gcc": rgb([162,255,105]),
        "Avx2-Pgi": rgb([255,93,171]),
        "Avx2-Clang": rgb([51,161,0]),
        "Altivec-Ibm": rgb([30,0,33]),
        "Altivec-Gcc": rgb([255,166,69]),
        "Altivec-Pgi": rgb([1,92,135]),
        "Altivec-Clang": rgb([1,92,135]),
        "Avx512-Gcc": rgb([150,89,0]),
        "Avx512-Icc": rgb([99,183,80]),
        "Avx512-Clang": rgb([121,0,7]),
        "Avx512-Pgi": rgb([121,0,7]),
        "Knl-Gcc": rgb([150,89,0]),
        "Knl-Icc": rgb([99,183,80]),
        "Knl-Clang": rgb([121,0,7]),
        "Knl-Pgi": rgb([121,0,7]),

        "Linear Dependence" : rgb([218,145,51]),
        "Induction Variable": rgb([89,112,216]),
        "Global Data Flow": rgb([180,179,53]),
        "Control Flow": rgb([164,91,207]),
        "Symbolics": rgb([99,183,80]),
        "Statement Reordering": rgb([202,73,160]),
        "Loop Restructuring": rgb([78,182,152]),
        "Node Splitting": rgb([ 215,60,102]),
        "Expansion": rgb([85,122,52]),
        "Crossing Thresholds" : rgb([206,139,203]),
        "Reductions": rgb([174,159,89]),
        "Recurrences": rgb([121,96,164]),
        "Searching": rgb([206,77,51]),
        "Packing": rgb([94,155,213]),
        "Loop Rerolling": rgb([146,93,39]),
        "Equivalencing": rgb([224,122,139]),
        "Indirect Addressing": rgb([220,136,102]),
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



def radar_factory(num_vars, frame='circle'):
    """Create a radar chart with `num_vars` axes.

    This function creates a RadarAxes projection and registers it.

    Parameters
    ----------
    num_vars : int
        Number of variables for radar chart.
    frame : {'circle' | 'polygon'}
        Shape of frame surrounding axes.

    """
    # calculate evenly-spaced axis angles
    theta = np.linspace(0, 2*np.pi, num_vars, endpoint=False)
    # rotate theta such that the first axis is at the top
    theta += np.pi/2

    def draw_poly_patch(self):
        verts = unit_poly_verts(theta)
        return plt.Polygon(verts, closed=True, edgecolor='k')

    def draw_circle_patch(self):
        # unit circle centered on (0.5, 0.5)
        return plt.Circle((0.5, 0.5), 0.5)

    patch_dict = {'polygon': draw_poly_patch, 'circle': draw_circle_patch}
    if frame not in patch_dict:
        raise ValueError('unknown value for `frame`: %s' % frame)

    class RadarAxes(PolarAxes):

        name = 'radar'
        # use 1 line segment to connect specified points
        RESOLUTION = 1
        # define draw_frame method
        draw_patch = patch_dict[frame]

        def fill(self, *args, **kwargs):
            """Override fill so that line is closed by default"""
            closed = kwargs.pop('closed', True)
            return super(RadarAxes, self).fill(closed=closed, *args, **kwargs)

        def plot(self, *args, **kwargs):
            """Override plot so that line is closed by default"""
            lines = super(RadarAxes, self).plot(*args, **kwargs)
            for line in lines:
                self._close_line(line)

        def _close_line(self, line):
            x, y = line.get_data()
            # FIXME: markers at x[0], y[0] get doubled-up
            if x[0] != x[-1]:
                x = np.concatenate((x, [x[0]]))
                y = np.concatenate((y, [y[0]]))
                line.set_data(x, y)

        def set_varlabels(self, labels):
        #    self.set_thetagrids(np.degrees(theta), labels,fontsize='small')
             self.set_thetagrids(np.degrees(theta), labels, frac=1.2)

        def _gen_axes_patch(self):
            return self.draw_patch()

        def _gen_axes_spines(self):
            if frame == 'circle':
                return PolarAxes._gen_axes_spines(self)
            # The following is a hack to get the spines (i.e. the axes frame)
            # to draw correctly for a polygon frame.

            # spine_type must be 'left', 'right', 'top', 'bottom', or `circle`.
            spine_type = 'circle'
            verts = unit_poly_verts(theta)
            # close off polygon by repeating first vertex
            verts.append(verts[0])
            path = Path(verts)

            spine = Spine(self, spine_type, path)
            spine.set_transform(self.transAxes)
            return {'polar': spine}

    register_projection(RadarAxes)
    return theta


def unit_poly_verts(theta):
    """Return vertices of polygon for subplot axes.

    This polygon is circumscribed by a unit circle centered at (0.5, 0.5)
    """
    x0, y0, r = [0.5] * 3
    verts = [(r*np.cos(t) + x0, r*np.sin(t) + y0) for t in theta]
    return verts

def plot_radar_chart(categories, values, labels, outputfile, title="", size=(8,8)):
    N = len(categories)
    theta = radar_factory(N, frame='polygon')

    for v in values:
        if len(v) != N:
            raise ValueError('expencting series of ' + str(N) + ' values')
    case_data = values

    spoke_labels = [x.replace(' ','\n') for x in categories]

    fig, ax = plt.subplots(subplot_kw=dict(projection='radar'))
    #fig.subplots_adjust(wspace=0.25, hspace=0.20, top=0.85, bottom=0.05)

    
    colorm = cmx.Set1.colors
    colors = {'gcc':colorm[0],
              'clang':colorm[1],
              'pgi':colorm[2],
              'icc':colorm[3],
              'ibm':colorm[4]
              }
    linestyles = {'gcc':':',
                'clang':'--',
                'pgi':'-.',
                'icc':'-',
                'ibm':'-'
                }
    llabels = {'gcc':'GCC 8.1',
               'clang':'Clang 6.0',
               'pgi':'PGI 18.4',
               'icc':'Intel ICC 2018u4',
               'ibm':'IBM XLC 13.5'
               }

    # Plot the four cases from the example data on separate axes
    #for ax, (title, case_data) in zip(axes.flatten(), data):
    #ax.set_rgrids([00,,1,2,3,4,5,6,7,8])
    #ax.set_title(title, weight='bold', size='medium', position=(0.5, 1.1),
    #              horizontalalignment='center', verticalalignment='center')
    for d, l in zip(case_data, labels):
        ax.plot(theta, d, color=colors[l], linestyle=linestyles[l])
        #ax.fill(theta, d, facecolor=color, alpha=0.25)
    ax.set_varlabels(spoke_labels)

    ax.set_rmin(0)
    if title.startswith('Altivec'):
        ax.set_rmax(16)
    elif title.startswith('AVX2'):
        ax.set_rmax(16)
    elif title.startswith('AVX512'):
        ax.set_rmax(16)
    elif title.startswith('KNL'):
        ax.set_rmax(16)
    # add legend relative to top-left plot
    #ax = axes[0, 0]
    #labels = ('Factor 1', 'Factor 2')
    l1 = Line2D([1,1],[2,2], linestyle=linestyles['gcc'], color=colors['gcc'], label=llabels['gcc'])
    l2 = Line2D([1,1],[2,2], linestyle=linestyles['clang'], color=colors['clang'], label=llabels['clang'])
    l3 = Line2D([1,1],[2,2], linestyle=linestyles['pgi'], color=colors['pgi'], label=llabels['pgi'])
    l4 = Line2D([1,1],[2,2], linestyle=linestyles['icc'], color=colors['icc'], label=llabels['icc'])
    l5 = Line2D([1,1],[2,2], linestyle=linestyles['ibm'], color=colors['ibm'], label=llabels['ibm'])
    #legend = ax.legend(handles=[l1,l2,l3,l4,l5], loc=(0.9,0.95), labelspacing=0.1, ncol=5)

    #fig.text(0.5, 0.965, '5-Factor Solution Profiles Across Four Scenarios',
    #         horizontalalignment='center', color='black', weight='bold',
    #         size='large')

    fig.set_size_inches(size[0],size[1])
    plt.savefig(outputfile, dpi=100, bbox_inches='tight', format='eps')


def add_box(ax, name, values, labels, ymax, draw_mean=False):

    if len(list(values)) != len(list(labels)):
        print("Error: Inconsisten number of values/labels")
        exit(-1)
    
    for value, label in zip(values, labels):
        if not np.isnan(value):
            ax.axhline(value, 0, 1, color=palette[label], label=label)

    if draw_mean:
        mean = np.mean([v for v in values if not np.isnan(v)])
        ax.axhline(mean,0,1,color='black',linestyle="--")
        ax.text(0.5, mean+0.05, "Avg. = " + "{:.2f}".format(mean), ha='center', va='bottom', fontsize=8)
    
    if max(values) > ymax:
        print("Error: value out of chart axis" )
        exit(0)
    
    ax.set_ylim(bottom=0, top=ymax)
    ax.tick_params(axis='x', which='both', bottom='off', top='off', labelbottom='off')
    ax.set_xlabel(name.title().replace("_","\n"), rotation=0, fontsize='small')

def plot_chart(charts, labels, values, outputfile, title= "Auto-vectorization",
        ylabel='Vector efficiency', connect=False, draw_mean=False, size=(4,4), ymax=8 ):

    if len(list(values)) != len(list(charts)):
        print("Error: Inconsistent number of charts/values")
        print("Values:",values)
        print("charts:",charts)
        exit(-1)

    
    if False:
        print(outputfile)
        #print(charts)
        #print(labels)
        #print(values)
        for idx, lab in enumerate(labels):
            print(lab,  round((values[0][idx]/values[1][idx] - 1)*100,1) )

    #Find existing elements
    fig, axis = plt.subplots(1,len(charts) + 1)

    #for ax in axis:
    #    ax.set_facecolor('none')

    if max(map(max,values)) > ymax:
        ymax = max(map(max,values)) + 1
        print("Warning: Reset the chart ymax to", ymax )
    
    for ax, c, v in zip(axis[:-1],list(charts),list(values)):
        add_box(ax, c, v, labels, ymax,  draw_mean) 

    for idx, (val,lab) in enumerate(sorted(zip(values[-1],labels))):
        v_loc = idx*(float(1)/len(labels))
        axis[-1].text(0.5,v_loc,lab, ha='left', va='center', fontsize=10)
        if connect:
            xy = (1,val)
            xy2 = (0.45,v_loc)
            con = ConnectionPatch(xyA=xy, xyB=xy2, coordsA="data",coordsB="data", axesA=axis[-2], axesB=axis[-1], color=palette[lab], connectionstyle="arc,angleA=-180,angleB=-180,armA=-15,armB=15,rad=0")
            axis[-2].add_artist(con)
        else:
            con = ConnectionPatch( xyA=(0.25,v_loc), xyB=(0.45,v_loc),
                           coordsA="data", coordsB="data",
                           axesA=axis[-1], axesB=axis[-1],
                           color=palette[lab])
            axis[-1].add_artist(con)

    

    #for ax in axis:
    #    ax.set_facecolor('none')

   
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

    #fig.suptitle(title)
    fig.set_size_inches(size[0],size[1])
    plt.savefig(outputfile, dpi=100, bbox_inches='tight', format='eps')


def getfolders(path):
    return filter(lambda x: os.path.isdir(os.path.join(path,x)),os.listdir(path))

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
            vecf = open(os.path.join(parameters_path,vecfname.replace('0',str(i))),'r')
            novecf = open(os.path.join(parameters_path,novecfname.replace('0',str(i))),'r')
            vecresults.append(vecf.readlines())
            novecresults.append(novecf.readlines())
    except FileNotFoundError:
        if file_number == 0:
            print("Warning, files ", os.path.join(parameters_path,vecfname), " or ", os.path.join(parameters_path,novecfname), " not found!" )
        else:
            print("Warning: Not all files found in ", parameters_path)

    for indx, (linevec, linenovec) in enumerate(zip(vecresults[0],novecresults[0])):
        if linevec[0] == 'S':
            if linenovec.split()[0] == linevec.split()[0]:
                test, novec_perf, cs1 = linenovec.split()
                test, vec_perf, cs2 = linevec.split()

                vecvalues = [float(item[indx].split()[1]) for item in vecresults]
                novecvalues = [float(item[indx].split()[1]) for item in novecresults]

                if len(vecvalues) > 1:
                    vecstd = statistics.stdev(vecvalues)
                    novecstd = statistics.stdev(novecvalues)

                vecmin = min(vecvalues); vecmax = vecmin + vecmin * 0.5
                novecmin = min(novecvalues); novecmax = novecmin + novecmin * 0.5

                newvecvalues = [x for x in vecvalues if x < vecmax]
                newnovecvalues = [x for x in novecvalues if x < novecmax]

                vec_perf = vecmin
                novec_perf = novecmin

                if novec_perf > 10 or vec_perf < 0.2:
                    pass
                    #print("--->", compiler, test, vec_perf, novec_perf)

                if test in remove_tests:
                    continue
                if abs(float(cs1) - float(cs2)) > abs(float(cs1)*0.01):
                    print("Warning checksums differ! ", compiler,
                            category, parameters, test, cs1," ",cs2)
                elif float(novec_perf) == 0.0 or float(vec_perf) == 0.0:
                    print("Warning contains 0 " , compiler,
                            category, parameters, test)
                elif len(newvecvalues) < 4 or len(newnovecvalues) < 4:
                    pass
                    #print("Warning 2 or more results eliminated as noise outliers ", len(newvecvalues) ,
                    #      " and ", len(newnovecvalues), " ", compiler, category, parameters, test)
                elif vecstd > 0.2 or novecstd > 0.2:
                    pass
                    #print("Warning standard deviation = ", vecstd ," and ", novecstd , compiler,
                    #        category, parameters, test)
                #elif float(novec_perf)/float(vec_perf) > (16.0 + 1.0):
                #    print("Warning outlier " , compiler, category,
                #        parameters, test, float(novec_perf),
                #        float(vec_perf), float(novec_perf)/float(vec_perf))
                #else: # --> use else to eliminate warrining cases
                data[compiler][category][parameters][test] = [
                    float(vec_perf),
                    float(novec_perf),
                    float(novec_perf)/float(vec_perf)
                    ]
            else:
                print("Warning, some lines are different!")

def plot_original_tsvc(data, output, architecture, title="", speedup_vs=None):
    # all compilers, all categories, original tsc
    vals = []
    labs = []
    
    compiler_list = [ x for x in data.keys() if architecture in x]
    
    for cat in [x for x in all_categories if x not in remove_categories] : 
        labs.append(cat)
        vals2 = []
        for comp in compiler_list :
            par = 'None' #TODO: The TSVC original was None -> all compile time

            if speedup_vs != None:
                baseline = np.mean([x[0] for x in data[speedup_vs][cat][par].values()])
                vals2.append(baseline / np.mean([x[0] for x in data[comp][cat][par].values()]))
            else:
                vals2.append(np.mean([x[2] for x in data[comp][cat][par].values()]))
        vals.append(vals2)

    vals = [list(i) for i in zip(*vals)]
    labs = [x.title().replace("_"," ") for x in labs]
    char = [x.split("-")[1]  for x in compiler_list]

    if True:
        print("Original TSVC for ", architecture)
        #print(vals)
        #print(labs)
        #print(char)

        for idx, compiler in enumerate(char):
            ordered_labeled_values = sorted(zip(labs,vals[idx]),key=lambda x:x[1])
            print(compiler)
            print(list(ordered_labeled_values))
            print("Average=", round(statistics.mean(vals[idx]),1), "  Median=", round(statistics.median(vals[idx]),1))
            print("")

    path = os.path.join(os.path.join('plots','originaltsvc_vspectrum'),output)
    plot_chart(char, labs , vals, path, title=title, ylabel = "Vector Efficiency", connect=False, draw_mean=True, size=(5,4))


    # order with clang-avx2 increasing vector efficiencies.
    ordered_labs = ['Recurrences', 'Statement Reordering', 'Searching', 'Packing', 'Loop Restructuring', 'Node Splitting', 'Crossing Thresholds', 'Loop Rerolling', 'Indirect Addressing', 'Linear Dependence', 'Expansion', 'Control Flow', 'Equivalencing', 'Induction Variable', 'Global Data Flow', 'Symbolics', 'Reductions']
    ordered_values = []
    for idx, compiler in enumerate(char):
        ordered_values.append([ x[1] for x in sorted(zip(labs, vals[idx]), key= lambda x: ordered_labs.index(x[0]))])
    
    #print(ordered_values)

    path = os.path.join('plots','originaltsvc_radars')
    path = os.path.join(os.path.join('plots','originaltsvc_radars'),output)
    plot_radar_chart(ordered_labs, ordered_values, char, path, title=title, size=(10,6))

def plot_categories(data, comp, output, title="", speedup=False):
    print(title)
    labs = []
    vals = []
    char = (
            'None',
            'RUNTIME_ATTRIBUTES',
            'RUNTIME_INDEX',
            'RUNTIME_CONDITIONS',
            'RUNTIME_LOOP_BOUNDS',
            'RUNTIME_ALL')

    chars_labs = ('Evrything\nexposed to\nthe compiler',
                  'Indices\nparameters\nhidden',
                  'Arithmetic\nparameters\nhidden',
                  'Loop\nbounds\nhidden',
                  'Variable\nattributes\nhidden',
                  'All\ninformation\nhidden')

    for cat in [x for x in data[comp].keys() if x not in remove_categories] : 
        labs.append(cat)
        vals2 = []
        for par in char:
            #print(par, "->" , data[comp][cat].keys())
            if speedup:
                baseline = np.mean([x[0] for x in data[comp][cat]['None'].values()])
                value = baseline / np.mean([x[0] for x in data[comp][cat][par].values()])
            else:
                vals2.append(np.mean([x[2] for x in data[comp][cat][par].values()]))
        vals.append(vals2)

    vals = [list(i) for i in zip(*vals)]
    labs = [x.title().replace("_"," ") for x in labs]


    plot_chart(char, labs , vals, output, title=title, ylabel = "Vector Efficiency", connect=False, draw_mean=True, size=(7,4),ymax=8)


def plot_new(data, detailed_summary, parameter, output, title="", speedup=False):
    labs = []

    # Average of all tests
    vals_ct = []
    vals_rt = []
    
    # Average of all categories averages
    cat_vals_ct = []
    cat_vals_rt = []

    for compiler in data.keys():
        #if compiler.startswith('knl') : continue
        #if compiler.endswith('pgi') : continue
        #if compiler == 'altivec-ibm' : continue
        labs.append(compiler)

        # Set total counters to 0
        none_sum = 0
        par_sum = 0
        count = 0

        for category in data[compiler].keys():

            cat_none_sum = 0
            cat_par_sum = 0
            cat_count = 0

            for test, value in data[compiler][category]['None'].items():
                #if (parameter not in test_sets) or (test in test_sets[parameter]): 
                if True: 
                    value_ct = value[2]
                    try:
                        rt = data[compiler][category][parameter][test]
                    except KeyError:
                        print ("Error: " + compiler + " " + category + " " + parameter \
                                + " " + test + " does not exist.")
                    value_rt = rt[2]

                    #if value_rt > value_ct + 0.1:
                    #    print("Warrning: Test " + str(test) + " has better veff with RT")
                    #    print("CT: vec " + str(value[0]) + " novec " \
                    #            + str(value[1]) + " veff " + str(value_ct)) 
                    #    print("RT: vec " + str(rt[0]) + " novec " \
                    #            + str(rt[1]) + " veff " + str(value_rt)) 

                    if value_rt == 0.0 or value_ct == 0.0 :
                        print("Error: " + str(test) + " contains 0")
                        exit(0)

                    cat_none_sum = cat_none_sum + value_ct
                    cat_par_sum = cat_par_sum + value_rt
                    cat_count = cat_count + 1

            # Save category averages on detailed_summary
            if(cat_count==0):
                print("Error: " + compiler + " " + parameter + " " + category + " count is 0")
                detailed_summary[compiler]['None'][category] = 0
                detailed_summary[compiler][parameter][category] = 0
            else:
                detailed_summary[compiler]['None'][category] = float(cat_none_sum/cat_count)
                detailed_summary[compiler][parameter][category] = float(cat_par_sum/cat_count)

            none_sum = none_sum + cat_none_sum
            par_sum = par_sum + cat_par_sum
            count = count + cat_count

        # All categories done
        #print("\n" + compiler + " with " + parameter + " has " + str(count) + " entries.")


        # Aggregate platform-compiler pair results
        if(count == 0):
            print("Warrning!: " + str(compiler)+ " NONE contains no tests. Set to 0")
            vals_ct.append(0)
            vals_rt.append(0)
        else:
            vals_ct.append(none_sum/count)
            vals_rt.append(par_sum/count)

    char = ('Known at\nCompile Time', 'Hidden at\nCompile Time')
    vals = [vals_ct,vals_rt]      
    labs = [x.title() for x in labs]

    #print(char,labs,vals)
    plot_chart(char, labs , vals, output, title=title, ylabel = "Vector Efficiency", connect=True,ymax=4)

def to_string(f):
    if f == 0.0:
        return "-"
    else:
        return "{:2.1f}".format(f)

def print_summary(data):
    categories = all_categories

    # Potentially change names and order
    parameters =  ['None','RUNTIME_ATTRIBUTES', 'RUNTIME_INDEX',
            'RUNTIME_LOOP_BOUNDS', 'RUNTIME_CONDITIONS','RUNTIME_ALL']

    mappars = {
            'None'                  :'Known at ct.',
            'RUNTIME_ATTRIBUTES'    :'rt. Attributes',
            'RUNTIME_INDEX'         :'rt. Indices',
            'RUNTIME_LOOP_BOUNDS'   :'rt. L. Bounds',
            'RUNTIME_CONDITIONS'    :'rt. Conditions' ,
            'RUNTIME_ALL'           :'rt. All'
            }
            

    rotation = 90
    cat_num = len(categories)
    par_num = len(parameters)

    fname = os.path.join(os.path.join('plots','latex_table'),'output.tex')
    with open(fname,'w') as f:
        f.write("\\documentclass{article}\n")
        f.write("\\usepackage{array,longtable}\n")
        f.write("\\usepackage{multicol}\n")
        f.write("\\usepackage{multirow}\n")
        f.write("\\begin{document}\n")

        # Begin table 1 with altivec and avx2
        f.write("\\begin{longtable}{")
        f.write(("|p{2cm}"+"|c"*(9))+"|}\n")
        f.write("\\cline{3-10} \multicolumn{2}{c|}{}")
        f.write(" & \multicolumn{4}{|c|}{ Altivec (on Power8)}")
        f.write(" & \multicolumn{4}{|c|}{ AVX2 (on Skylake)} \\\\\n")

        f.write("\\cline{3-10} \multicolumn{2}{c|}{}")
        f.write("& GNU & Clang & PGI & IBM ")
        f.write("& GNU & Clang & PGI & Intel \\\\ \\hline\n")
        #f.write("\\endhead\n")

        for cat in categories:
            f.write("\\multirow{" + str(par_num) + "}{*}{ \parbox{2cm}{" +
                    cat.replace('_','\\\\').title() + "}}")
            for par in parameters:
                f.write(" & " + mappars[par])
                for c in ['altivec-gcc','altivec-clang','altivec-pgi',
                        'altivec-ibm','avx2-gcc','avx2-clang','avx2-pgi',
                        'avx2-icc']:
                    f.write(" & " + "{:3.1f}".format(np.mean(
                        [v[2] for v in data[c][cat][par].values()]
                        )))
                f.write("\\\\ \\cline{2-10}\n")
            f.write("\\hline\n")
        f.write("\\end{longtable}\n")

        # Begin table 1 with altivec and avx2
        f.write("\\begin{longtable}{")
        f.write(("|p{2cm}"+"|c"*(9))+"|}\n")
        f.write("\\cline{3-10} \multicolumn{2}{c|}{}")
        f.write(" & \multicolumn{4}{|c|}{ AVX512 (on Skylake)}")
        f.write(" & \multicolumn{4}{|c|}{ AVX512 (on KNL)} \\\\\n")

        f.write("\\cline{3-10} \multicolumn{2}{c|}{}")
        f.write("& GNU & Clang & PGI & Intel ")
        f.write("& GNU & Clang & PGI & Intel \\\\ \\hline\n")
        #f.write("\\endhead\n")

        for cat in categories:
            f.write("\\multirow{" + str(par_num) + "}{*}{ \parbox{2cm}{" +
                    cat.replace('_','\\\\').title() + "}}")
            for par in parameters:
                f.write(" & " + mappars[par])
                for c in ['avx512-gcc','avx512-clang','avx512-pgi',
                        'avx512-icc','knl-gcc','knl-clang','knl-pgi',
                        'knl-icc']:
                    f.write(" & " + "{:3.1f}".format(np.mean(
                        [v[2] for v in data[c][cat][par].values()]
                        )))
                f.write("\\\\ \\cline{2-10}\n")
            f.write("\\hline\n")
        f.write("\\end{longtable}\n")



        f.write("\\end{document}\n")



def data_sanity_check(data):

    #Nested dictionary of: compiler, category, parameters, test 
    
    for compiler in sorted(data.keys()):
        tests_per_category = []
        total = 0

        # Check all compilers have all categories
        if len(data[compiler].keys()) != 17 :
            print ("Error: " + compiler +" missing categories")
            print (data[compiler].keys())
            exit(0)
        else:
            for category in data[compiler].keys():
                if category not in all_categories:
                    print("Error: " + compiler + " unknown category " + category)
                    exit(0)

        for category in data[compiler].keys():
            if len(data[compiler][category].keys()) != 7:
                print ("Error: "+ compiler + " " + category + " missing parameters")
                print(data[compiler][category].keys())
                continue
                #exit(0)
            else:
                for parameter in data[compiler][category].keys():
                    if parameter not in all_parameters:
                        print ("Error: "+ compiler + " " + category + " missing parameters")
                        exit(0)

            for parameter in data[compiler][category].keys():
                if data[compiler][category][parameter].keys() != \
                   data[compiler][category]['None'].keys():
                        print("Error: missmatching parameters "+compiler+" "+category+" "+parameter)
                        print(data[compiler][category][parameter].keys())
                        print(data[compiler][category]['None'].keys())
                        #exit(0)
                if len(data[compiler][category][parameter].keys()) < 1 :
                        print("Error: Empty "+compiler+" "+category+" "+parameter)
                        exit(0)

            total = total + len(data[compiler][category]['None'].keys())
            tests_per_category.append(len(data[compiler][category]['None'].keys()))

        print(compiler, total, tests_per_category)



    if False: # Print veff differences btw information classes
        for compiler in sorted(data.keys()):
            for category in data[compiler].keys():
                par1 = 'None'
                par2 = 'RUNTIME_CONDITIONS'
                tests = data[compiler][category][par1].keys()

                for t in tests:
                    par1veff = data[compiler][category][par1][t][2]
                    par2veff = data[compiler][category][par2][t][2]
                    if (par1veff+4) < par2veff:
                        print(compiler, category)
                        print(t, data[compiler][category][par1][t], data[compiler][category][par2][t])



def main():


    # Get argv 1 and error checking
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
    os.makedirs(os.path.join('plots','originaltsvc_radars'))
    os.makedirs(os.path.join('plots','originaltsvc_vspectrum'))
    os.makedirs(os.path.join('plots','extendedtsvc_summary'))
    os.makedirs(os.path.join('plots','extendedtsvc_detailed'))
    os.makedirs(os.path.join('plots','latex_table'))


    #Nested dictionary of: compiler, category, parameters, test : [performance vec, performance novec, vector eff]
    data = defaultdict(lambda : defaultdict(lambda :defaultdict(dict)))
    #Nested dictionary of: compiler, parameter, category : avg_vector_eff
    detailed_summary = defaultdict(lambda : defaultdict(lambda :defaultdict(float)))

    print("Loading data...")
    for compiler in getfolders(datadir):
        print(compiler)
        compiler_path = os.path.join(datadir,compiler)
        for category in all_categories: #getfolders(compiler_path):
            category_path = os.path.join(compiler_path,category)
            for parameters in all_parameters: #getfolders(category_path):
                parameters_path = os.path.join(category_path,parameters)
                load_data(data, compiler,category,parameters,parameters_path)
    

    print("\nData sanity check...")
    data_sanity_check(data)
    print("\nWriting summary to file...")
    print_summary(data)
    #exit(0)
    
    print("\nPloting Summary VSpectrums..")
    path = os.path.join('plots','extendedtsvc_summary')
    plot_new(data, detailed_summary, 'RUNTIME_INDEX',
            os.path.join(path,'index_parameters.eps'), 'Index Parameters')
    plot_new(data, detailed_summary, 'RUNTIME_LOOP_BOUNDS',
            os.path.join(path,'loop_bound.eps'), 'Loop Bounds Parameters')
    plot_new(data, detailed_summary, 'RUNTIME_CONDITIONS',
        os.path.join(path,'conditional_parameters.eps'), 'Conditional Parameters')
    plot_new(data, detailed_summary, 'RUNTIME_ARITHMETIC',
        os.path.join(path,'arithmetic_parameters.eps'), 'Arithmetic Parameters')
    plot_new(data, detailed_summary, 'RUNTIME_ATTRIBUTES',
        os.path.join(path,'variable_attributes.eps'), 'Variable attributes')
    plot_new(data, detailed_summary, 'RUNTIME_ALL',
        os.path.join(path,'all.eps'), 'All Parameters')
    exit()

    #plt.close("all")
    #print("- Compiler comparison")
    #plot_original_tsvc(data, 'compilers-avx2.eps', 'avx2', 'AVX2 Compiler comparison')
    #plot_original_tsvc(data, 'compilers-avx512.eps', 'avx512', 'AVX512 Compiler comparison')
    #plot_original_tsvc(data, 'compilers-knl.eps', 'knl', 'KNL Compiler comparison')
    #plot_original_tsvc(data, 'compilers-altivec.eps', 'altivec', 'Altivec Compiler comparison')
    exit()

    #plt.close("all")

    print("- Detailed VSpectrums")
    path = os.path.join('plots','extendedtsvc_detailed')
    plot_categories(data, 'avx2-icc', os.path.join(path,'avx2-icc.eps'), title="AVX2 ICC Auto-vectorization", speedup=False)
    plot_categories(data, 'avx2-gcc', os.path.join(path,'avx2-gcc.eps'), title="AVX2 GCC Auto-vectorization", speedup=False)
    plot_categories(data, 'avx2-pgi', os.path.join(path,'avx2-pgi.eps'), title="AVX2 PGI Auto-vectorization", speedup=False)
    plot_categories(data, 'avx2-clang', os.path.join(path,'avx2-clang.eps'), title="AVX Clang Auto-vectorization", speedup=False)

    plot_categories(data, 'avx512-icc', os.path.join(path,'avx512-icc.eps'), title="AVX512 ICC Auto-vectorization", speedup=False)
    plot_categories(data, 'avx512-gcc', os.path.join(path,'avx512-gcc.eps'), title="AVX512 GCC Auto-vectorization", speedup=False)
    plot_categories(data, 'avx512-pgi', os.path.join(path,'avx512-pgi.eps'), title="AVX512 PGI Auto-vectorization", speedup=False)
    plot_categories(data, 'avx512-clang', os.path.join(path,'avx512-clang.eps'), title="AVX512 Clang Auto-vectorization", speedup=False)

    plt.close("all")
    plot_categories(data, 'knl-icc', os.path.join(path,'knl-icc.eps'), title="KNL ICC Auto-vectorization", speedup=False)
    plot_categories(data, 'knl-gcc', os.path.join(path,'knl-gcc.eps'), title="KNL GCC Auto-vectorization", speedup=False)
    plot_categories(data, 'knl-pgi', os.path.join(path,'knl-pgi.eps'), title="KNL PGI Auto-vectorization", speedup=False)
    plot_categories(data, 'knl-clang', os.path.join(path,'knl-clang.eps'), title="KNL Clang Auto-vectorization", speedup=False)

    plot_categories(data, 'altivec-gcc', os.path.join(path,'altivec-gcc.eps'), title="Altivec GCC Auto-vectorization", speedup=False)
    plot_categories(data, 'altivec-ibm', os.path.join(path,'altivec-ibm.eps'), title="Altivec IBM Auto-vectorization", speedup=False)
    plot_categories(data, 'altivec-pgi', os.path.join(path,'altivec-pgi.eps'), title="Altivec PGI Auto-vectorization", speedup=False)
    plot_categories(data, 'altivec-clang', os.path.join(path,'altivec-clang.eps'), title="Altivec Clang Auto-vectorization", speedup=False)


    # TODO: also add ISA comparsion and architecture comparison

if __name__ == "__main__":
    main()
