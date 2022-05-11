from json.tool import main
import matplotlib.pyplot as plt
from collections import defaultdict

import numpy as np


def plot_insertion_size():
    with open("size_experiments_insertions_without_dup_large.csv") as f:
        data = [tuple(map(int, line.strip().split(","))) for line in f.readlines()[1:]]

    tem_size_sum = defaultdict(lambda: [])
    per_size_sum = defaultdict(lambda: [])

    for _, n, tem, per in data:
        tem_size_sum[n].append(tem)
        per_size_sum[n].append(per)

    def points(size_sum):
        return sorted((n, sum(values) / len(values) / n) for n, values in size_sum.items())


    plt.plot(*zip(*points(tem_size_sum)), "o:", label="Temporal size")
    plt.plot(*zip(*points(per_size_sum)), "o:", label="Persistent size")

    plt.title("Average Space Experiment\nWith only Insertion Updates")
    plt.xlabel("Number of Insertions")
    plt.ylabel("Space usage (bytes) / Insertions")

    plt.xscale("log")

    plt.legend()
    plt.show()
    # plt.savefig("plots/size_insertion_without_duplicates.pdf")


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
    with open("bst_unbalanced_update_insert_total_time_FULL.csv") as f:
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

    increase = [(n, t) for n, times in batch_increase.items() for t in times]
    avg_increase = [(n, sum(times) / len(times)) for n, times in batch_increase.items()]

    plt.plot(*zip(*increase), ".", color="black")
    plt.plot(*zip(*avg_increase), "o:", color="red")
    
    plt.title("Average Update Time Increase Experiment\nUnbalanced BST with only random Insertion Updates")
    plt.xlabel("Number of Insertions")
    plt.ylabel("Persistent Runtime / Temporal Runtime")

    plt.xscale("log")

    plt.show()


if __name__ == "__main__":
    # plot_insertion_size()
    # plot_deletion_size()
    # plot_deletion_size_with_node_splits()
    plot_update_runtime()
