import matplotlib.pyplot as plt
from collections import defaultdict


with open("size_experiments.csv") as f:
    data = [tuple(map(int, line.strip().split(","))) for line in f.readlines()[1:]]

tem_size_sum = defaultdict(lambda: [])
per_size_sum = defaultdict(lambda: [])

for n, tem, per in data:
    tem_size_sum[n].append(tem)
    per_size_sum[n].append(per)

def points(size_sum):
    return sorted((n, sum(values) / len(values)) for n, values in size_sum.items())


plt.plot(*zip(*points(tem_size_sum)), "o:", label="Temporal size")
plt.plot(*zip(*points(per_size_sum)), "o:", label="Persistent size")

plt.title("Space Experiment")
plt.xlabel("Number of insertions")
plt.ylabel("Space usage (bytes)")

plt.xscale("log")

plt.legend()
plt.show()
