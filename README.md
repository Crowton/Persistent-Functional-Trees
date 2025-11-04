# Offline Partial Persistent of Functional Trees

This repository contains an implemtation of the framework created in *Space-Efficient Functional Offline-Partially-Persistent Trees with Applications to Planar Point Location* by Brodal, Rysgaard, Schou, and Svenning.
The paper is [available here](http://doi.org/10.1007/978-3-031-38906-1\_43).

Specifically, the framework allows for turning a tree structure into an offline persistent tree with efficient update, query, and space bounds.
The *offline* criteria allows for operations in three phases:

1. Update
2. Build
3. Query

The framework consist of three files, supporting converting an ephemeral tree into an offline partial persistent version, corresponding to the three phases

1. [PersistentUpdate.hs](PersistentUpdate.hs)
2. [DAGconstruction.hs](DAGconstruction.hs)
3. [PersistentQuery.hs](PersistentQuery.hs)

As an example of usage, this repository contains ephemeral and partially persistent versions of binary trees, red-black trees, and Random Access Lists.

For an example of how this framework is convinient, a simple ephemeral binary tree can be found in [BinaryTreeEphemeral.hs](BinaryTreeEphemeral.hs). This has then been made offline persistent directly ([BinaryTreePersistentMock.hs](BinaryTreePersistentMock.hs)) and using the framework functions ([BinaryTreePersistent.hs](BinaryTreePersistent.hs)). Note the difference in complexity of the code, and the resemplence to the ephemeral code.

## Citation

The paper:
```
@inproceedings{tuna,
  author       = {Gerth St{\o}lting Brodal and
                  Casper Moldrup Rysgaard and
                  Jens Kristian Refsgaard Schou and
                  Rolf Svenning},
  editor       = {Pat Morin and
                  Subhash Suri},
  title        = {Space-Efficient Functional Offline-Partially-Persistent Trees with
                  Applications to Planar Point Location},
  booktitle    = {Algorithms and Data Structures - 18th International Symposium, {WADS}
                  2023, Montreal, QC, Canada, July 31 - August 2, 2023, Proceedings},
  series       = {Lecture Notes in Computer Science},
  volume       = {14079},
  pages        = {644--659},
  publisher    = {Springer},
  year         = {2023},
  doi          = {10.1007/978-3-031-38906-1\_43}
}
```

This code:
```
@misc{TUNA_impl,
  author = {Gerth St{\o}lting Brodal and
            Casper Moldrup Rysgaard and
            Jens Kristian Refsgaard Schou and
            Rolf Svenning},
  url = {https://github.com/Crowton/Persistent-Functional-Trees},
  title = {Persistent-Functional-Trees},
  year = {2022}
}
```