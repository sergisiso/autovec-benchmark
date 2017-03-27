#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt

N = 4
values = (1,2,3,4)
colors = ('r', 'c', 'm', 'y', 'k', 'b', 'g', 'r', 'c', 'm')

labels = ( "LINEAR_DEPENDENCE", "INDUCTION_VARIABLE", "GLOBAL_DATA_FLOW"
        "CONTROL_FLOW", "SYMBOLICS", "STATEMENT_REORDERING",
        "LOOP_RESTRUCTURING", "NODE_SPLITTING", "EXPANSION",
        "CROSSING_THRESHOLDS", "REDUCTIONS", "RECURRENCES",
        "SEARCHING", "PACKING", "LOOP_REROLLING", "EQUIVALENCING",
        "INDIRECT_ADDRESSING", "CONTROL_LOOPS")

charts = ("ALL", "RUNTIME_LOOP_BOUNDS_PARAMETERS", "NONE"
          "RUNTIME_ARITHMETIC_PARAMETERS", "RUNTIME_INDEX_PARAMETERS",
          "CONDITION_EVAL_PARAMETERS")



def add_box(ax, name, values, labels, colors):
    for value, label, color in zip(values, labels, colors):
        ax.bar(0, 0.05, width=1, bottom=value, color=color, linewidth=0)
        ax.text(0.5, value+0.10, label.title().replace("_"," "), ha='center', va='bottom')

    ax.set_ylim(bottom=1, top=8)
    ax.tick_params(axis='x', which='both', bottom='off', top='off', labelbottom='off')
    ax.set_xlabel(name.title().replace("_"," "))


    pass

def plot_chart():

    #Find existing elements

    fig, axis = plt.subplots(1,2)

    for i, ax in enumerate(axis):
        add_box(ax, charts[i], values, labels, colors[:len(values)]) 

    #ax.plot([0., 4.5], [0.4, 3.4], "k--")

    axis[0].set_ylabel('Vector efficiency')
    fig.suptitle('Test auto-vectorization with hidden information')

    #plt.show()
    plt.savefig('test.png')

plot_chart()
