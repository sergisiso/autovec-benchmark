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

debug = False


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

    def unit_poly_verts(theta):
        """Return vertices of polygon for subplot axes.

        This polygon is circumscribed by a unit circle centered at (0.5, 0.5)
        """
        x0, y0, r = [0.5] * 3
        verts = [(r*np.cos(t) + x0, r*np.sin(t) + y0) for t in theta]
        return verts

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
            # self.set_thetagrids(np.degrees(theta), labels,fontsize='small')
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


def plot_radar_chart(categories, values, labels, outputfile, title="",
                     size=(8, 8)):
    N = len(categories)
    theta = radar_factory(N, frame='polygon')

    for v in values:
        if len(v) != N:
            raise ValueError('expencting series of ' + str(N) + ' values')
    case_data = values

    spoke_labels = [x.replace(' ', '\n') for x in categories]

    fig, ax = plt.subplots(subplot_kw=dict(projection='radar'))
    # fig.subplots_adjust(wspace=0.25, hspace=0.20, top=0.85, bottom=0.05)

    colorm = cmx.Set1.colors
    colors = {'gcc': colorm[0],
              'clang': colorm[1],
              'pgi': colorm[2],
              'icc': colorm[3],
              'ibm': colorm[4]
              }
    linestyles = {'gcc': ':',
                  'clang': '--',
                  'pgi': '-.',
                  'icc': '-',
                  'ibm': '-'
                  }

    llabels = {'gcc': 'GCC 8.1',
               'clang': 'Clang 6.0',
               'pgi': 'PGI 18.4',
               'icc': 'Intel ICC 2018u4',
               'ibm': 'IBM XLC 13.5'
               }

    # Plot the four cases from the example data on separate axes
    # for ax, (title, case_data) in zip(axes.flatten(), data):
    # ax.set_rgrids([00,,1,2,3,4,5,6,7,8])
    # ax.set_title(title, weight='bold', size='medium', position=(0.5, 1.1),
    #              horizontalalignment='center', verticalalignment='center')
    for d, l in zip(case_data, labels):
        ax.plot(theta, d, color=colors[l], linestyle=linestyles[l])
        # ax.fill(theta, d, facecolor=color, alpha=0.25)
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
    # ax = axes[0, 0]
    # labels = ('Factor 1', 'Factor 2')
    l1 = Line2D([1, 1], [2, 2], linestyle=linestyles['gcc'],
                color=colors['gcc'], label=llabels['gcc'])
    l2 = Line2D([1, 1], [2, 2], linestyle=linestyles['clang'],
                color=colors['clang'], label=llabels['clang'])
    l3 = Line2D([1, 1], [2, 2], linestyle=linestyles['pgi'],
                color=colors['pgi'], label=llabels['pgi'])
    l4 = Line2D([1, 1], [2, 2], linestyle=linestyles['icc'],
                color=colors['icc'], label=llabels['icc'])
    l5 = Line2D([1, 1], [2, 2], linestyle=linestyles['ibm'],
                color=colors['ibm'], label=llabels['ibm'])

    # legend = ax.legend(handles=[l1,l2,l3,l4,l5], loc=(0.9, 0.95),
    #                    labelspacing=0.1, ncol=5)

    # fig.text(0.5, 0.965, '5-Factor Solution Profiles Across Four Scenarios',
    #         horizontalalignment='center', color='black', weight='bold',
    #         size='large')

    fig.set_size_inches(size[0], size[1])
    plt.savefig(outputfile, dpi=100, bbox_inches='tight', format='eps')


def add_box(ax, name, values, labels, ymin, ymax, draw_mean=False):

    if len(list(values)) != len(list(labels)):
        print("Error: Inconsisten number of values/labels")
        exit(-1)

    for value, label in zip(values, labels):
        if not np.isnan(value):
            ax.axhline(value, 0, 1, color=palette[label], label=label)

    if draw_mean:
        mean = np.mean([v for v in values if not np.isnan(v)])
        ax.axhline(mean, 0, 1, color='black', linestyle="--")
        ax.text(0.5, mean+0.05, "Avg. = " + "{:.2f}".format(mean),
                ha='center', va='bottom', fontsize=8)

    if max(values) > ymax:
        print("Error: value out of chart axis")
        exit(0)

    ax.set_ylim(bottom=ymin, top=ymax)
    ax.tick_params(axis='x', which='both', bottom='off', top='off',
                   labelbottom='off')
    ax.set_xlabel(name.title().replace("_", "\n"), rotation=0,
                  fontsize='small')


def plot_vspectrum(charts, labels, values, outputfile,
                   title="", ylabel="Vector efficiency", connect=False,
                   draw_mean=False, size=(4, 4), ymin=0, ymax=8):

    if len(list(values)) != len(list(charts)):
        print("Error: Inconsistent number of charts/values")
        print("Values:", values)
        print("charts:", charts)
        exit(-1)

    if debug:
        print(outputfile)
        # print(charts)
        # print(labels)
        # print(values)
        for idx, lab in enumerate(labels):
            print(lab, round((values[0][idx]/values[1][idx]-1)*100, 1))

    # Find existing elements
    fig, axis = plt.subplots(1, len(charts) + 1)

    # for ax in axis:
    #    ax.set_facecolor('none')

    if max(map(max, values)) > ymax:
        ymax = max(map(max, values)) + 1
        print("Warning: Reset the chart ymax to", ymax)

    for ax, c, v in zip(axis[:-1], list(charts), list(values)):
        add_box(ax, c, v, labels, ymin, ymax,  draw_mean)

    for idx, (val, lab) in enumerate(sorted(zip(values[-1], labels))):
        v_loc = idx*(float(1)/len(labels))
        axis[-1].text(0.5, v_loc, lab, ha='left', va='center', fontsize=10)
        if connect:
            xy = (1, val)
            xy2 = (0.45, v_loc)
            con = ConnectionPatch(xyA=xy, xyB=xy2, coordsA="data",
                                  coordsB="data", axesA=axis[-2],
                                  axesB=axis[-1], color=palette[lab],
                                  connectionstyle="arc,angleA=-180,"
                                  "angleB=-180,armA=-15,armB=15,rad=0")
            axis[-2].add_artist(con)
        else:
            con = ConnectionPatch(xyA=(0.25, v_loc), xyB=(0.45, v_loc),
                                  coordsA="data", coordsB="data",
                                  axesA=axis[-1], axesB=axis[-1],
                                  color=palette[lab])
            axis[-1].add_artist(con)

    # for ax in axis:
    #    ax.set_facecolor('none')

    if len(values) > 1 and connect:
        # Connect same labels among inner charts (just 0 to 1 implemented)
        for val1, val2, lab in zip(values[0], values[1], labels):
            con = ConnectionPatch(xyA=(1, val1), xyB=(0, val2),
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

    # We need to draw the canvas, otherwise the labels won't be positioned
    # and won't have values yet.
    fig.canvas.draw()

    labels = axis[0].get_yticks().tolist()
    # print(labels)
    labels[-1] = '(AVX512 vector length) ' + str(labels[-1])
    # print(labels)
    axis[0].set_xticklabels(labels)

    # fig.suptitle(title)
    fig.set_size_inches(size[0], size[1])
    plt.savefig(outputfile, dpi=100, bbox_inches='tight', format='eps')
