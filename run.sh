set -e

# ghc DAG_construction.hs -o DAG
# ./DAG

# ghc Query.hs -o Query
# ./Query

# -odir -hidir   (Set out for .o and .hi files)
# -fhpc          (Generate .tix with code coverage counters)

# -O2 -fno-full-laziness

ghc Main.hs -o Main -odir out -hidir out  # -O2
./Main

# ghc Binary_Tree_mock.hs -o Binary_Tree_mock
# ./Binary_Tree_mock
