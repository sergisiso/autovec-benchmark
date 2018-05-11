#!/usr/bin/env python3

import sys
import os
from collections import defaultdict
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import ConnectionPatch
from matplotlib.path import Path
from matplotlib.spines import Spine
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
        "Altivec-Xlc": rgb([30,0,33]),
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
            self.set_thetagrids(np.degrees(theta), labels,fontsize='small')

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

    spoke_labels = categories

    fig, ax = plt.subplots(subplot_kw=dict(projection='radar'))
    #fig.subplots_adjust(wspace=0.25, hspace=0.20, top=0.85, bottom=0.05)

    colors = ['b', 'r', 'g', 'm', 'y']
    # Plot the four cases from the example data on separate axes
    #for ax, (title, case_data) in zip(axes.flatten(), data):
    #ax.set_rgrids([00,,1,2,3,4,5,6,7,8])
    ax.set_title(title, weight='bold', size='medium', position=(0.5, 1.1),
                  horizontalalignment='center', verticalalignment='center')
    for d, color in zip(case_data, colors):
        ax.plot(theta, d, color=color)
        #ax.fill(theta, d, facecolor=color, alpha=0.25)
    ax.set_varlabels(spoke_labels)

    #ax.set_rmin(0); ax.set_rmax(8)
    # add legend relative to top-left plot
    #ax = axes[0, 0]
    #labels = ('Factor 1', 'Factor 2')
    legend = ax.legend(labels, loc=(0.9,0.95),
                       labelspacing=0.1)

    #fig.text(0.5, 0.965, '5-Factor Solution Profiles Across Four Scenarios',
    #         horizontalalignment='center', color='black', weight='bold',
    #         size='large')

    fig.set_size_inches(size[0],size[1])
    plt.savefig(outputfile, dpi=100, bbox_inches='tight')


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
        ylabel='Vector efficiency', connect=False, draw_mean=False, size=(4,4) ):

    if len(list(values)) != len(list(charts)):
        print("Error: Inconsistent number of charts/values")
        exit(-1)

    #Find existing elements
    fig, axis = plt.subplots(1,len(charts) + 1)

    #for ax in axis:
    #    ax.set_facecolor('none')

    ymax = 8
    if max(map(max,values)) > ymax:
        ymax = max(map(max,values))
        print("Warning: Reset the chart ymax to", ymax )
    
    for ax, c, v in zip(axis[:-1],list(charts),list(values)):
        add_box(ax, c, v, labels, ymax,  draw_mean) 

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

    fig.suptitle(title)
    fig.set_size_inches(size[0],size[1])
    plt.savefig(outputfile, dpi=100, bbox_inches='tight')


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
    try:
        with open(os.path.join(parameters_path,vecfname)) as vecf:
            with open(os.path.join(parameters_path,novecfname)) as novecf:
                for linevec, linenovec in zip(vecf.readlines(),novecf.readlines()):
                    if linevec[0] == 'S':
                        if linenovec.split()[0] == linevec.split()[0]:
                            test, novec_perf, cs1 = linenovec.split()
                            test, vec_perf, cs2 = linevec.split()
                            if test in remove_tests:
                                continue
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
                            #else: # --> use else to eliminate warrining cases
                            data[compiler][category][parameters][test] = [
                                float(vec_perf),
                                float(novec_perf),
                                float(novec_perf)/float(vec_perf)
                                ]
                        else:
                            print("Warning, some lines are different!")
    except FileNotFoundError:
        print("Warning, files", os.path.join(parameters_path,vecfname), " or ", os.path.join(parameters_path,novecfname), " not found!" )

def plot_compilers(data, output, architecture, title="", speedup_vs=None):
    # all compilers, all categories, original tsc
    vals = []
    labs = []
    
    compiler_list = [ x for x in data.keys() if architecture in x]
    
    for cat in [x for x in all_categories if x not in remove_categories] : 
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
    char = [x.split("-")[1]  for x in compiler_list]

    #print("charts",char)
    #print("LABS", labs)
    #print("vals", vals)
    plot_chart(char, labs , vals, output, title=title, ylabel = "Vector Efficiency", connect=False, draw_mean=True, size=(5,4))

    plot_radar_chart(labs,vals,char,'radar_'+output, title=title, size=(10,6))

def plot_categories(data, comp, output, title="", speedup=False):
    labs = []
    vals = []
    char = ('None', 'RUNTIME_INDEX',
            'RUNTIME_ARITHMETIC', 'RUNTIME_LOOP_BOUNDS', 'RUNTIME_ALL')

    for cat in [x for x in data[comp].keys() if x not in remove_categories] : 
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



    plot_chart(char, labs , vals, output, title=title, ylabel = "Vector Efficiency", connect=False, draw_mean=True, size=(7,4))


def plot_new(data, detailed_summary, parameter, output, title="", speedup=False):
    labs = []

    # Average of all tests
    vals_ct = []
    vals_rt = []
    
    # Average of all categories averages
    cat_vals_ct = []
    cat_vals_rt = []

    for compiler in data.keys():
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
                        print ("Error:" + compiler + category + parameter \
                                + test + " does not exist")
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

    char = ('Compile Time \n Known', 'Compile Time \n Hidden')
    vals = [vals_ct,vals_rt]      
    labs = [x.title() for x in labs]

    #print(char,labs,vals)
    plot_chart(char, labs , vals, output, title=title, ylabel = "Vector Efficiency", connect=True)

def to_string(f):
    if f == 0.0:
        return "-"
    else:
        return "{:2.1f}".format(f)

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
        f.write("\\begin{tabular}{|c|c" + ("|c"*(cat_num)) +"|}\n")
        f.write("\\hline\n")
        f.write("\\begin{turn}{"+str(rotation)+"}Architecture-Compiler\\end{turn} & ")
        #f.write("\\begin{turn}{"+str(rotation)+"}Compiler\\end{turn} & ")
        f.write("\\begin{turn}{"+str(rotation)+"}Parameters\\end{turn} ")
        for cat in categories:
            f.write(" & \\begin{turn}{"+str(rotation)+"}"+cat.title().replace("_"," ") +"\\end{turn} ")

        f.write("\\\\ \n")

        for archcomp in sorted(detailed_summary.keys()):
            arch, comp = archcomp.split("-")
            #print(arch)
            #print(comp)
            f.write("\\hline \n")
            f.write("  \multirow{"+str(par_num)+"}{*}{\\begin{turn}{90}"+archcomp+"\\end{turn}} & CT ALL &") 
            f.write(" & ".join([to_string(detailed_summary[archcomp]['None'][x]) for x in categories])+" \\\\ \n")
            f.write(" & RT LOOP BOUNDS & ")
            f.write(" & ".join([to_string(detailed_summary[archcomp]['RUNTIME_LOOP_BOUNDS'][x]) for x in categories])+" \\\\ \n")
            f.write(" & RT INDICES & ")
            f.write(" & ".join([to_string(detailed_summary[archcomp]['RUNTIME_INDEX'][x]) for x in categories])+" \\\\ \n")
            f.write(" & RT CONDITIONALS & ")
            f.write(" & ".join([to_string(detailed_summary[archcomp]['RUNTIME_CONDITIONS'][x]) for x in categories])+" \\\\ \n")
            f.write(" & RT ARITHMETIC & ")
            f.write(" & ".join([to_string(detailed_summary[archcomp]['RUNTIME_ARITHMETIC'][x]) for x in categories])+" \\\\ \n")
            f.write(" & RT ALL & ")
            f.write(" & ".join([to_string(detailed_summary[archcomp]['RUNTIME_ALL'][x]) for x in categories])+" \\\\ \n")
        f.write("\\hline \n")
        f.write("\\end{tabular}\n")
        f.write("\\end{adjustbox}\n")


        #f.write("{}")
        #f.write("Header: "+ str(categories))
        #for compiler in detailed_summary.keys():
        #    f.write(compiler)
        #    for parameter in detailed_summary[compiler].keys():
        #        values = [str(x) + " " +str(detailed_summary[compiler][parameter][x]) for x in categories ]
        #        f.write(parameter + " : " + str(values))
 

def data_sanity_check(data):

    #Nested dictionary of: compiler, category, parameters, test 
    
    print("\nData Summary:")
    for compiler in sorted(data.keys()):
        tests_per_category = []
        total = 0

        # Check all compilers have all categories
        if len(data[compiler].keys()) != 17 :
            print ("Error: " + compiler +" missing categories")
            exit(0)
        else:
            for category in data[compiler].keys():
                if category not in all_categories:
                    print("Error: " + compiler + " unknown category " + category)
                    exit(0)

        for category in data[compiler].keys():
            if len(data[compiler][category].keys()) != 6:
                print ("Error: "+ compiler + category + " missing parameters")
                print(data[compiler][category].keys())
                exit(0)
            else:
                for parameter in data[compiler][category].keys():
                    if parameter not in all_parameters:
                        print ("Error: "+ compiler + category + " missing parameters")
                        exit(0)

            for parameter in data[compiler][category].keys():
                if data[compiler][category][parameter].keys() != \
                   data[compiler][category]['None'].keys():
                        print("Error: missmatching parameters "+compiler+category+parameter)
                        print(data[compiler][category][parameter].keys())
                        print(data[compiler][category]['None'].keys())
                        exit(0)
                if len(data[compiler][category][parameter].keys()) < 1 :
                        print("Error: Empty "+compiler+category+parameter)
                        exit(0)

            total = total + len(data[compiler][category]['None'].keys())
            tests_per_category.append(len(data[compiler][category]['None'].keys()))

        print(compiler, total, tests_per_category)




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
    
    #data_sanity_check(data)
    print("")

    print("Ploting Parameters charts...")
    plot_new(data, detailed_summary, 'RUNTIME_INDEX','rindex.png', 'Index Parameters')
    plot_new(data, detailed_summary, 'RUNTIME_LOOP_BOUNDS','rbound.png', 'Loop Bounds Parameters')
    plot_new(data, detailed_summary, 'RUNTIME_CONDITIONS','rcond.png', 'Conditional Parameters')
    plot_new(data, detailed_summary, 'RUNTIME_ARITHMETIC','rarith.png', 'Arithmetic Parameters')
    plot_new(data, detailed_summary, 'RUNTIME_ALL','rall.png', 'All Parameters')
    print_summary(detailed_summary, all_categories)

    #exit(0)
    print("- Compiler comparison")
    plot_compilers(data, 'compilers-avx2.png', 'avx2', 'Broadwell Compiler comparison')
    plot_compilers(data, 'compilers-altivec.png', 'altivec', 'Power8 Compiler comparison')
    plot_compilers(data, 'compilers-avx512.png', 'avx512', 'KNL Compiler comparison')
    plot_compilers(data, 'platforms-gcc.png', 'gcc', 'GCC Platform comparison')
    #plot_compilers(data, 'compilers_avx2_vs_gcc.png', 'avx2', 'Broadwell Performance against gcc', speedup_vs='avx2-gcc')
    #plot_compilers(data, 'compilers_altivec_vs_gcc.png', 'altivec', 'Power8 Performance against gcc', speedup_vs='alitvec-gcc')
    #plot_compilers(data, 'compilers_avx512_vs_gcc.png', 'avx512', 'KNL Performance against gcc', speedup_vs='avx512-gcc')

    print("- Categories comparison")
    plot_categories(data, 'avx2-icc', 'avx2-icc.png', title="BDW ICC Auto-vectorization", speedup=False)
    plot_categories(data, 'avx2-gcc', 'avx2-gcc.png', title="BDW GCC Auto-vectorization", speedup=False)
    plot_categories(data, 'avx2-pgi', 'avx2-pgi.png', title="BDW PGI Auto-vectorization", speedup=False)
    plot_categories(data, 'altivec-gcc', 'altivec-gcc.png', title="Power8 GCC Auto-vectorization", speedup=False)
    plot_categories(data, 'altivec-xlc', 'altivec-xlc.png', title="Power8 XLC Auto-vectorization", speedup=False)
    plot_categories(data, 'avx512-icc', 'avx512-icc.png', title="KNL ICC Auto-vectorization", speedup=False)
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
