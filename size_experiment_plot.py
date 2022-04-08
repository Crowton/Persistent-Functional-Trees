from json.tool import main
import matplotlib.pyplot as plt
from collections import defaultdict

import numpy as np


def plot_insertion_experiment():
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


def plot_deletion_experiment():
    with open("size_experiments_worst_case_deletions_large.csv") as f:
        data = [tuple(map(int, line.strip().split(","))) for line in f.readlines()[1:]]

    # c = 100000
    # data_per = [(3 * n, per / (3 * n + c)) for n, per in data]

    a, b = 159.98040958, -567.82009912

    data_per = [(3 * n, per / (3*n + b / a)) for n, per in data]

    # x, y = zip(*data_per)
    # coef = np.polyfit(x, y, 1)
    # x_values = np.linspace(min(x), max(x), 10000)
    # y_values = np.polyval(coef, x_values)
    # plt.plot(x_values, y_values, "r-")

    # print(coef)

    # y_bar = np.mean(y)
    # ss_res = np.sum((y - np.polyval(coef, x)) ** 2)
    # ss_tot = np.sum((y - y_bar) ** 2)
    # r_2 = 1 - ss_res / ss_tot
    # print(r_2)

    plt.plot(*zip(*data_per), "o:", label="Persistent size")

    plt.title("Node Splitting Space Experiment\nWith $2n$ Worst case Insertions and $n$ Deletions")
    plt.xlabel("Number of Updates")
    plt.ylabel("Space usage (bytes) / Updates")

    plt.xscale("log")

    plt.legend()
    plt.show()


if __name__ == "__main__":
    # plot_insertion_experiment()
    plot_deletion_experiment()
