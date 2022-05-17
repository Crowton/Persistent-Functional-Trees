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


def plot_insertion_deletion_worst_case_size_with_node_splits():
    with open("data/bst_unbalanced_space_insert_and_delete_worst_case_splits_only_persistent.csv") as f:
        data = [tuple(map(int, line.strip().split(","))) for line in f.readlines()[1:]]
        data = [(3 * n, size, splits) for n, size, splits in data]

    size_pr_size = [(n, size / n) for n, size, _ in data]
    splits_pr_size = [(n, splits / n) for n, _, splits in data]

    plt.suptitle("Space Experiment\nUnbalanced BST with Worst case Insertion and Deletion Updates")

    plt.subplot(2, 1, 1)
    plt.plot(*zip(*size_pr_size), "o:")
    plt.title("Space")
    plt.xlabel("Number of Updates")
    plt.ylabel("Space usage (bytes) / Updates")
    plt.xscale("log")

    plt.subplot(2, 1, 2)
    plt.plot(*zip(*splits_pr_size), "o:")
    plt.title("Splits")
    plt.xlabel("Number of Updates")
    plt.ylabel("Splits / Updates")
    plt.xscale("log")

    plt.show()


def plot_insertion_deletion_worst_case_range_size_with_node_splits():
    with open("data/bst_unbalanced_space_insert_and_delete_worst_case_splits_range_only_persistent.csv") as f:
        data = [tuple(map(int, line.strip().split(","))) for line in f.readlines()[3:]]
        data = [(3 * n, size, splits) for n, size, splits in data]

    size_pr_size = [(n, size / n) for n, size, _ in data]
    splits_pr_size = [(n, splits / n) for n, _, splits in data]

    plt.suptitle("Space Experiment\nUnbalanced BST with Worst case Insertion and Deletion Updates")

    plt.subplot(2, 1, 1)
    plt.plot(*zip(*size_pr_size), ".")
    plt.title("Space")
    plt.xlabel("Number of Updates")
    plt.ylabel("Space usage (bytes) / Updates")
    # plt.xscale("log")

    plt.subplot(2, 1, 2)
    plt.plot(*zip(*splits_pr_size), ".")
    plt.title("Splits")
    plt.xlabel("Number of Updates")
    plt.ylabel("Splits / Updates")
    # plt.xscale("log")

    plt.show()


def plot_insertion_deletion_worst_case_range_size_node_splits():
    with open("data/bst_unbalanced_space_insert_and_delete_worst_case_splits_range_only_persistent.csv") as f:
        data = [tuple(map(int, line.strip().split(","))) for line in f.readlines()[3:]]
        # data = [(3 * n, size, splits) for n, size, splits in data]

    split_diff = [s2 - s1 for (_, _, s1), (_, _, s2) in zip(data, data[1:])]

    plt.plot(split_diff, ".")
    plt.show()


def plot_update_runtime(path, title):
    with open(path) as f:
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
    
    plt.title(title)
    plt.xlabel("Number of Updates")
    plt.ylabel("Persistent Runtime / Temporal Runtime")

    plt.xscale("log")
    plt.ylim(ymin=0)

    plt.legend()
    plt.show()


def plot_RB_update_insertion_time():
    with open("data/bst_RB_update_insert_range.csv") as f:
        data = [tuple(line.strip().split(",")) for line in f.readlines()[1:]]
        data = [(int(n), float(tem), float(per)) for n, tem, per in data]

    batch_times = defaultdict(lambda: [])
    for n, tem, per in data:
        batch_times[n].append((tem, per))
    
    avg_batch_times = [
        (n, *[sum(times) / len(times) for times in zip(*values)])
        for n, values in batch_times.items()
    ]

    # all_batch_increase = [
    #     (n, p / t)
    #     for n, values in batch_times.items()
    #     for (tem_times, per_times) in [zip(*values)]
    #     for t in tem_times
    #     for p in per_times
    # ]

    # all_batch_increase = []
    # for n, values in batch_times.items():
    #     tem, per = zip(*values)
    #     for t, p in zip(sorted(tem), sorted(per)):
    #         all_batch_increase.append((n, p / t))

    batch_increase = sorted([(n, per_avg / tem_avg) for n, tem_avg, per_avg in avg_batch_times])
    
    # plt.plot(*zip(*all_batch_increase), ".", color="black", label="Ratio from all times")
    plt.plot(*zip(*batch_increase), "o:", color="red", label="Ratio from Average Ratio of times")
    
    plt.title("Update Time Increase Experiment\nRed-Black BST with Increasing Insertion Updates")
    plt.xlabel("Number of Updates")
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


def plot_query_worst_case_insert_delete_contains_leaf():
    # with open("data/bst_unbalanced_query_wort_case_insert_delete_contains_leaf.csv") as f:
    with open("data/bst_unbalanced_query_wort_case_insert_delete_contains_leaf_LARGER.csv") as f:
        data = [tuple(line.strip().split(",")) for line in f.readlines()[1:]]
        data = [(int(time), float(tem), float(per)) for time, tem, per in data]

    tem_times = defaultdict(lambda: [])
    per_times = defaultdict(lambda: [])

    for time, tem, per in data:
        tem_times[time].append(tem)
        per_times[time].append(per)

    def points(times):
        return sorted((n, v) for n, values in times.items() for v in values)

    def avg_points(times):
        return sorted((n, sum(values) / len(values)) for n, values in times.items())


    plt.plot(*zip(*points(tem_times)), ".", label="Temporal time", color="Black")
    plt.plot(*zip(*avg_points(tem_times)), "o:", label="Average Temporal time", color="Blue")
    plt.plot(*zip(*points(per_times)), ".", label="Persistent time", color="Grey")
    plt.plot(*zip(*avg_points(per_times)), "o:", label="Average Persistent time", color="Orange")

    plt.title("Query Time Experiment\nUnbalanced BST path with repeated Insertion and Deletion of leaf\nQuery containing of leaf")
    plt.xlabel("Version Queried")
    plt.ylabel("Runtime (s)")

    plt.xscale("log")
    # plt.ylim(ymin=0)

    plt.legend()
    plt.show()


def plot_query_relative_worst_case_insert_delete_contains_leaf():
    with open("data/bst_unbalanced_query_wort_case_insert_delete_contains_leaf.csv") as f:
        data_small = [tuple(line.strip().split(",")) for line in f.readlines()[1:]]
        data_small = [(int(time), float(tem), float(per)) for time, tem, per in data_small]
    
    with open("data/bst_unbalanced_query_wort_case_insert_delete_contains_leaf_LARGER.csv") as f:
        data_large = [tuple(line.strip().split(",")) for line in f.readlines()[1:]]
        data_large = [(int(time), float(tem), float(per)) for time, tem, per in data_large]

    def process(data):
        batch_times = defaultdict(lambda: [])
        for time, tem, per in data:
            batch_times[time].append((tem, per))

        avg_batch_times = [
            (time, *[sum(times) / len(times) for times in zip(*values)])
            for time, values in batch_times.items()
        ]

        return [(time, per / tem) for time, tem, per in avg_batch_times]

    plt.plot(*zip(*process(data_small)), "o:", label="Path length 1000")
    plt.plot(*zip(*process(data_large)), "o:", label="Path length 3000")

    plt.title("Query Time Experiment\nUnbalanced BST path with repeated Insertion and Deletion of leaf\nQuery containing of leaf")
    plt.xlabel("Version Queried")
    plt.ylabel("Persistent Runtime / Temporal Runtime")

    plt.xscale("log")
    plt.ylim(ymin=0)

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
    ### SIZE
    # plot_insertion_size()
    # plot_insertion_deletion_size()
    # plot_insertion_deletion_worst_case_size_with_node_splits()
    # plot_insertion_deletion_worst_case_range_size_with_node_splits()
    # plot_insertion_deletion_worst_case_range_size_node_splits()

    ### UPDATE
    # plot_update_runtime(
    #     "data/bst_unbalanced_update_insert_total_time_FULL.csv",
    #     "Update Time Increase Experiment\nUnbalanced BST with only random Insertion Updates"
    # )
    # plot_update_runtime(
    #     "data/bst_unbalanced_update_insert_and_delete_total_time.csv",
    #     "Update Time Increase Experiment\nUnbalanced BST with Insertion and Deletion Updates"
    # )

    # plot_RB_update_insertion_time()

    ### DAG BUILDING
    # plot_build_runtime()
    # plot_worst_case_build_runtime()

    ### QUERY
    # plot_query_worst_case_insert_delete_contains_leaf()
    plot_query_relative_worst_case_insert_delete_contains_leaf()

    # SANITY TIME
    # plot_sanity_test_runtime()
