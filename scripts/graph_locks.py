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
    (1000**-8, 'y'),
    (1000**-7, 'z'),
    (1000**-6, 'a'),
    (1000**-5, 'f'),
    (1000**-4, 'p'),
    (1000**-3, 'n'),
    (1000**-2, 'µ'),
    (1000**-1, 'm'),
    (1000**0,  ''),
    (1000**1,  'K'),
    (1000**2,  'M'),
    (1000**3,  'G'),
    (1000**4,  'T'),
    (1000**5,  'P'),
    (1000**6,  'E'),
    (1000**7,  'Z'),
    (1000**8,  'Y'),
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

FUNCTIONS = [
    'alloc',
    'dealloc',
    'post',
    'cancel',
    'dispatch',
]

def seconds(x, pos=None):
    for scale, si in reversed(SI):
        if x >= scale or (x == 0 and scale == 1):
            return '%s %ss' % (
                re.sub(r'\.0*$', '', ('%.3f'%(x/scale))[:3]), si)

def main(graph_path, *csv_paths):
    # find runtimes
    runtimes = []
    for csv_path in csv_paths:
        name, csv_path = csv_path.split('=', 1)
        single_runtimes = co.defaultdict(lambda: [])

        with open(csv_path) as f:
            for line in csv.DictReader(f):
                if line['group'] == 'locks':
                    assert line['unit'] == 'ns'
                    runtime = (float(line['sample_measured_value'])
                        / float(line['iteration_count'])
                        / 1000000000)
                    single_runtimes[line['function']].append(runtime)

        single_runtimes = {k: sum(v for v in vs) / len(vs) for k, vs in single_runtimes.items()}
        runtimes.append((name, [(f, throughput) for f, throughput in sorted(
            single_runtimes.items(),
            key=lambda x: FUNCTIONS.index(x[0])
        )]))

    # construct the graph
    matplotlib.rc('font', family='sans-serif', size=11)
    matplotlib.rc('axes', titlesize='medium', labelsize='medium')
    matplotlib.rc('xtick', labelsize='small')
    matplotlib.rc('ytick', labelsize='small')

    gs = gridspec.GridSpec(nrows=1, ncols=1, wspace=0.25, hspace=0.35)
    fig = plt.figure(figsize=(7, 3.5))

    ax = fig.add_subplot(gs[0, 0])
    ax.text(0.5, 1.025, 'Time locked', ha='center', transform=ax.transAxes)

    # plot each set of bars
    width = 1/(len(runtimes)+1)
    for i, (name, single_runtimes) in enumerate(runtimes):
        x = np.arange(len(single_runtimes))
        ax.bar(
            x - ((len(runtimes)-1)/2)*width + i*width,
            [runtime for _, runtime in single_runtimes],
            width=width,
            label=name,
            color=COLORS[i])

    # labels
    ax.set_xlim(0 - (len(runtimes)/2+1)*width, len(runtimes[0][1])-1 + (len(runtimes)/2+1)*width)
    ax.set_xticks(np.arange(len(runtimes[0][1])))
    ax.set_xticklabels([f for f, _ in runtimes[0][1]])
    ax.set_xlabel('operation')
    ax.set_ylim(0, None)
    ax.yaxis.set_major_formatter(FuncFormatter(seconds))
    ax.set_ylabel('seconds')

    # legend
    ax.legend(loc='upper left', bbox_to_anchor=(1, 1))

    # misc axis tweaks
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)

    fig.tight_layout()
    plt.savefig(graph_path, bbox_inches="tight")

if __name__ == "__main__":
    main(*sys.argv[1:])

