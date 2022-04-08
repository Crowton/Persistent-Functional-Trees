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

    data_per = [(3 * n, per) for n, per in data]
    data_per_div_update = [(3 * n, per / (3 * n)) for n, per in data]

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
    data_per_div_line = [(3 * n, per / (3 * n + b / a)) for n, per in data]
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
    with open("size_experiments_worst_case_deletions_large_with_node_split_count.csv") as f:
        data = [tuple(map(int, line.strip().split(","))) for line in f.readlines()[1:]]

    data_split = [(3 * n, splits / (3 * n)) for n, _, splits in data]

    plt.plot(*zip(*data_split), "o:")

    plt.title("Node Splitting Space Experiment\nWith $2n$ Worst case Insertions and $n$ Deletions")
    plt.xlabel("Number of Updates")
    plt.ylabel("Number of Node splits / Updates")

    plt.xscale("log")

    plt.show()


def plot_update_runtime():
    with open("initial_run_update_experiment.csv") as f:
        data = [tuple(map(int, line.strip().split(","))) for line in f.readlines()[1:]]

    tem_time_sum = defaultdict(lambda: [])
    per_time_sum = defaultdict(lambda: [])

    for _, n, tem, per in data:
        tem_time_sum[n].append(tem)
        per_time_sum[n].append(per)

    def points(time_sum):
        return sorted((n, sum(values) / len(values) / n / 1000000) for n, values in time_sum.items())


    plt.plot(*zip(*points(tem_time_sum)), "o:", label="Temporal time")
    plt.plot(*zip(*points(per_time_sum)), "o:", label="Persistent time")

    plt.title("Average Time Experiment\nWith only Insertion Updates")
    plt.xlabel("Number of Insertions")
    plt.ylabel("Total Time Usage (ms) / Insertions")

    plt.xscale("log")

    plt.legend()
    plt.show()


if __name__ == "__main__":
    # plot_insertion_size()
    # plot_deletion_size()
    # plot_deletion_size_with_node_splits()
    plot_update_runtime()
