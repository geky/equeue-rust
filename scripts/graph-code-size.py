#!/usr/bin/env python3

import sys
import json
import re
import csv
import itertools as it
import collections as co
import os

import matplotlib
matplotlib.use('SVG')
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from matplotlib.ticker import FuncFormatter
import numpy as np
import csv


# seaborn color palette
COLORS = [
    '#4c72b0',
    '#dd8452',
    '#55a868',
    '#c44e52',
    '#8172b3',
    '#937860',
    '#da8bc3',
    '#8c8c8c',
    '#ccb974',
    '#64b5cd'
]

# SI prefixes
SI = [
    (1000**0, ''),
    (1000**1, 'K'),
    (1000**2, 'M'),
    (1000**3, 'G'),
    (1000**4, 'T'),
    (1000**5, 'P'),
    (1000**6, 'E'),
    (1000**7, 'Z'),
    (1000**8, 'Y'),
]

# SI base2 prefixes
SI2 = [
    (1024**0, ''  ),
    (1024**1, 'Ki'),
    (1024**2, 'Mi'),
    (1024**3, 'Gi'),
    (1024**4, 'Ti'),
    (1024**5, 'Pi'),
    (1024**6, 'Ei'),
    (1024**7, 'Zi'),
    (1024**8, 'Yi'),
]

def throughput_bytes(x, pos=None):
    for scale, si in reversed(SI2):
        if x >= scale or scale == 1:
            return '%s %sB' % (
                re.sub(r'\.0*$', '', ('%.3f'%(x/scale))[:3]), si)

def main(graph_path, *csv_paths):
    # find code sizes
    code_sizes = []
    for csv_path in csv_paths:
        name, csv_path = csv_path.split('=', 1)
        single_code_size = 0

        with open(csv_path) as f:
            for line in csv.DictReader(f):
                single_code_size += int(line['size'])

        code_sizes.append((name, single_code_size))

    # construct the graph
    matplotlib.rc('font', family='sans-serif', size=11)
    matplotlib.rc('axes', titlesize='medium', labelsize='medium')
    matplotlib.rc('xtick', labelsize='small')
    matplotlib.rc('ytick', labelsize='small')

    gs = gridspec.GridSpec(nrows=1, ncols=1, wspace=0.25, hspace=0.35)
    fig = plt.figure(figsize=(1.5, 3.5))

    ax = fig.add_subplot(gs[0, 0])
    ax.text(0.5, 1.025, 'Code size', ha='center', transform=ax.transAxes)

    # plot each set of bars
    width = 1/(len(code_sizes)+1)
    for i, (name, single_code_size) in enumerate(code_sizes):
        x = np.arange(1)
        ax.bar(
            x - ((len(code_sizes)-1)/2)*width + i*width,
            single_code_size,
            width=width,
            label=name,
            color=COLORS[i])

    # labels
    ax.set_xlim(0 - (len(code_sizes)/2+1)*width, 0 + (len(code_sizes)/2+1)*width)
    ax.set_xticks([0])
    ax.set_xticklabels([''])
    ax.set_xlabel('code')
    ax.set_ylim(0, None)
    ax.yaxis.set_major_formatter(FuncFormatter(throughput_bytes))
    ax.set_ylabel('bytes')

    # legend
    ax.legend(loc='upper left', bbox_to_anchor=(1, 1))

    # misc axis tweaks
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)

    fig.tight_layout()
    plt.savefig(graph_path, bbox_inches="tight")

if __name__ == "__main__":
    main(*sys.argv[1:])

