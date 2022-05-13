from json.tool import main
import matplotlib.pyplot as plt
from collections import defaultdict

import numpy as np

from math import log2


def plot_insertion_size():
    with open("data/bst_unbalanced_space_inserts.csv") as f:
        data = [tuple(map(int, line.strip().split(","))) for line in f.readlines()[1:]]

    tem_size_sum = defaultdict(lambda: [])
    per_size_sum = defaultdict(lambda: [])

    for _, n, tem, per in data:
        tem_size_sum[n].append(tem)
        per_size_sum[n].append(per)

    def points(size_sum):
        return sorted((n, v / n) for n, values in size_sum.items() for v in values)

    def avg_points(size_sum):
        return sorted((n, sum(values) / len(values) / n) for n, values in size_sum.items())


    plt.plot(*zip(*points(tem_size_sum)), ".", label="Temporal size for fixed seed", color="Black")
    plt.plot(*zip(*avg_points(tem_size_sum)), "o:", label="Average Temporal size", color="Blue")
    plt.plot(*zip(*avg_points(per_size_sum)), "o:", label="Persistent size", color="Orange")

    plt.title("Space Experiment\nUnbalanced BST with only Insertion Updates")
    plt.xlabel("Number of Updates")
    plt.ylabel("Space usage (bytes) / Updates")

    plt.xscale("log")
    plt.ylim(ymin=0)

    plt.legend()
    plt.show()


def plot_insertion_deletion_size():
    with open("data/bst_unbalanced_space_insert_and_delete.csv") as f:
        data = [tuple(map(int, line.strip().split(","))) for line in f.readlines()[1:]]
        data = [(seed, 2 * n, tem, per) for seed, n, tem, per in data]

    tem_size_sum = defaultdict(lambda: [])
    per_size_sum = defaultdict(lambda: [])

    for _, n, tem, per in data:
        tem_size_sum[n].append(tem)
        per_size_sum[n].append(per)

    def points(size_sum):
        return sorted((n, v / n) for n, values in size_sum.items() for v in values)

    def avg_points(size_sum):
        return sorted((n, sum(values) / len(values) / n) for n, values in size_sum.items())


    plt.plot(*zip(*points(tem_size_sum)), ".", label="Temporal size for fixed seed", color="Black")
    plt.plot(*zip(*avg_points(tem_size_sum)), "o:", label="Average Temporal size", color="Blue")
    plt.plot(*zip(*points(per_size_sum)), ".", label="Persistent size for fixed seed", color="Grey")
    plt.plot(*zip(*avg_points(per_size_sum)), "o:", label="Average Persistent size", color="Orange")

    plt.title("Space Experiment\nUnbalanced BST with Insertion and Deletion Updates")
    plt.xlabel("Number of Updates")
    plt.ylabel("Space usage (bytes) / Updates")

    plt.xscale("log")
    plt.ylim(ymin=0)

    plt.legend()
    plt.show()


# NOT USED
def plot_deletion_size():
    with open("size_experiments_worst_case_deletions_large.csv") as f:
        data = [tuple(map(int, line.strip().split(","))) for line in f.readlines()[1:]]

    data_per = [(3 * n + 2, per) for n, per in data]
    data_per_div_update = [(n, per / n) for n, per in data_per]

    x, y = zip(*data_per)
    coef = np.polyfit(x, y, 1)
    print("Fitted coefficients:", coef)

    y_bar = np.mean(y)
    ss_res = np.sum((y - np.polyval(coef, x)) ** 2)
    ss_tot = np.sum((y - y_bar) ** 2)
    r_2 = 1 - ss_res / ss_tot
    print("R^2 value:", r_2)

    plt.plot(*zip(*data_per_div_update), "o:", label="Persistent size")

    plt.title("Node Splitting Space Experiment\nWith $2n$ Worst case Insertions and $n$ Deletions")
    plt.xlabel("Number of Updates")
    plt.ylabel("Space usage (bytes) / Updates")

    plt.xscale("log")

    plt.legend()
    plt.show()


    a, b = coef
    data_per_div_line = [(n, per / (n + b / a)) for n, per in data_per]
    x_values = np.linspace(min(x), max(x), 10000)
    y_values = np.polyval(coef, x_values)
    plt.plot(x_values, y_values / (x_values + b / a), "r-", label="Fitted line")

    plt.plot(*zip(*data_per_div_line), "o:", label="Persistent size")

    plt.title("Node Splitting Space Experiment\nWith $2n$ Worst case Insertions and $n$ Deletions")
    plt.xlabel("Number of Updates")
    plt.ylabel("Space usage (bytes) / (Updates + c)")

    plt.xscale("log")

    plt.legend()
    plt.show()


# NOT USED
def plot_deletion_size_with_node_splits():
    # with open("size_experiments_worst_case_deletions_large_with_node_split_count.csv") as f:
    with open("worst_case_delete_size_range.csv") as f:
        data = [tuple(map(int, line.strip().split(","))) for line in f.readlines()[1:]]

    data_split = [(3 * n + 2, splits / (3 * n + 2)) for n, _, splits in data]

    plt.plot(*zip(*data_split), "o:")

    plt.title("Node Splitting Space Experiment\nWith $2n$ Worst case Insertions and $n$ Deletions")
    plt.xlabel("Number of Updates")
    plt.ylabel("Number of Node splits / Updates")

    plt.xscale("log")

    plt.show()


def plot_update_runtime():
    with open("data/bst_unbalanced_update_insert_total_time_FULL.csv") as f:
        data = [tuple(line.strip().split(",")) for line in f.readlines()[1:]]
        data = [(int(seed), int(n), float(tem), float(per)) for seed, n, tem, per in data]

    batch_times = defaultdict(lambda: [])
    for seed, n, tem, per in data:
        batch_times[(seed, n)].append((tem, per))
    
    avg_batch_times = [
        (seed, n, *[sum(times) / len(times) for times in zip(*values)])
        for (seed, n), values in batch_times.items()
    ]

    batch_increase = defaultdict(lambda: [])
    for _, n, tem_avg, per_avg in avg_batch_times:
        batch_increase[n].append(per_avg / tem_avg)
        # batch_increase[n].append((per_avg - tem_avg) / tem_avg)

    sorted_batch_increase = sorted(batch_increase.items())
    increase = [(n, t) for n, times in sorted_batch_increase for t in times]
    avg_increase = [(n, sum(times) / len(times)) for n, times in sorted_batch_increase]

    plt.plot(*zip(*increase), ".", color="black", label="Ratio from Average Time over fixed seed")
    plt.plot(*zip(*avg_increase), "o:", color="red", label="Ratio from Average Ratio of seeds")
    
    plt.title("Update Time Increase Experiment\nUnbalanced BST with only random Insertion Updates")
    plt.xlabel("Number of Insertions")
    plt.ylabel("Persistent Runtime / Temporal Runtime")

    plt.xscale("log")
    plt.ylim(ymin=0)

    plt.legend()
    plt.show()


def plot_build_runtime():
    with open("data/bst_unbalanced_dag_build_time_insert_and_delete.csv") as f:
        data = [tuple(line.strip().split(",")) for line in f.readlines()[1:]]
        data = [(int(seed), 2 * int(n), float(time)) for seed, n, time in data]
        data = [(seed, n, time / n) for seed, n, time in data]

    batch_times = defaultdict(lambda: [])
    for seed, n, time in data:
        batch_times[(seed, n)].append(time)
    
    avg_batch_times = [
        (seed, n, sum(times) / len(times))
        for (seed, n), times in batch_times.items()
    ]

    avg_times = defaultdict(lambda: [])
    for _, n, time_avg in avg_batch_times:
        avg_times[n].append(time_avg)

    sorted_avg_times = sorted(avg_times.items())
    times = [(n, t) for n, times in sorted_avg_times for t in times]
    avg = [(n, sum(times) / len(times)) for n, times in sorted_avg_times]

    plt.plot(*zip(*times), ".", color="black", label="Average Time for fixed seed")
    plt.plot(*zip(*avg), "o:", color="red", label="Average Time over seeds")

    plt.title("DAG Build Time Experiment\nUnbalanced BST with random Insertion and Deletion Updates")
    plt.xlabel("Number of Updates")
    plt.ylabel("Runtime / Updates")

    plt.xscale("log")
    # plt.ylim(ymin=0)

    plt.legend()
    plt.show()


def plot_worst_case_build_runtime():
    with open("data/bst_unbalanced_dag_build_time_worst_case_insert_delete_leaf.csv") as f:
        data = [tuple(line.strip().split(",")) for line in f.readlines()[1:]]
        data = [(3 * int(n), float(time), int(splits)) for n, time, splits in data]
        data = [(n, time / n, splits) for n, time, splits in data]

    batch_times = defaultdict(lambda: [])
    for n, time, _ in data:
        batch_times[n].append(time)
    
    sorted_batch_times = sorted(batch_times.items())
    times = [(n, t) for n, times in sorted_batch_times for t in times]
    avg = [(n, sum(times) / len(times)) for n, times in sorted_batch_times]

    plt.plot(*zip(*times), ".", color="black", label="Times")
    plt.plot(*zip(*avg), "o:", color="red", label="Average Time")

    plt.title("DAG Build Time Experiment\nUnbalanced BST path with repeated Insertion and Deletion of leaf")
    plt.xlabel("Number of Updates")
    plt.ylabel("Runtime / Updates")

    plt.xscale("log")
    # plt.ylim(ymin=0)

    plt.legend()
    plt.show()


def plot_sanity_test_runtime():
    with open("data/sanity_time_test_bst_query_all.csv") as f:
        data = [tuple(line.strip().split(",")) for line in f.readlines()[1:]]
        data = [(int(seed), int(n), float(time)) for seed, n, time in data]
        data = [(seed, n, time / (n * log2(n) ** 4)) for seed, n, time in data]

    batch_times = defaultdict(lambda: [])
    for seed, n, time in data:
        batch_times[(seed, n)].append(time)
    
    avg_batch_times = [
        (seed, n, sum(times) / len(times))
        for (seed, n), times in batch_times.items()
    ]

    avg_times = defaultdict(lambda: [])
    for _, n, time_avg in avg_batch_times:
        avg_times[n].append(time_avg)

    sorted_avg_times = sorted(avg_times.items())
    times = [(n, t) for n, times in sorted_avg_times for t in times]
    avg = [(n, sum(times) / len(times)) for n, times in sorted_avg_times]

    plt.plot(*zip(*times), ".", color="black", label="Average Time for fixed seed")
    plt.plot(*zip(*avg), "o:", color="red", label="Average Time over seeds")

    plt.title("Sanity Time Experiment\nPerfect BST Query all nodes")
    plt.xlabel("Number of Queries (q)")
    plt.ylabel("Runtime / (q lg$^4$ q)")

    plt.xscale("log")
    plt.ylim(ymin=0)

    plt.legend()
    plt.show()



if __name__ == "__main__":
    # plot_insertion_size()
    # plot_insertion_deletion_size()
    # plot_update_runtime()
    # plot_build_runtime()
    # plot_worst_case_build_runtime()
    plot_sanity_test_runtime()
