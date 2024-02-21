benchmarking postgresSql/pgSelectRowsSequentially: {numQueries: 1}
time                 30.72 ms   (30.12 ms .. 31.39 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 30.65 ms   (30.47 ms .. 30.87 ms)
std dev              707.5 μs   (556.1 μs .. 822.1 μs)

benchmarking postgresSql/pgSelectAndUpdateRowsInParallel: {numTransactions : 1, numRowsPerTx: 5}
time                 950.9 ms   (751.1 ms .. 973.1 ms)
                     0.995 R²   (0.986 R² .. 1.000 R²)
mean                 1.012 s    (982.5 ms .. 1.037 s)
std dev              48.87 ms   (29.37 ms .. 57.71 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRowsInParallel: {numTransactions: 100, numRowsPerTx: 5}
time                 833.0 ms   (NaN s .. 1.287 s)
                     0.923 R²   (NaN R² .. 1.000 R²)
mean                 1.580 s    (1.270 s .. 1.913 s)
std dev              396.8 ms   (196.2 ms .. 541.9 ms)
variance introduced by outliers: 48% (moderately inflated)

benchmarking postgresSql/pgSelectRowsSequentially: {numQueries: 100}
time                 3.446 s    (2.831 s .. 3.644 s)
                     0.996 R²   (0.991 R² .. 1.000 R²)
mean                 3.741 s    (3.648 s .. 3.924 s)
std dev              184.2 ms   (144.5 ms .. 206.7 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectMultipleRows: {numQueries: 1, numRows: 500}
time                 37.18 ms   (36.35 ms .. 38.99 ms)
                     0.996 R²   (0.993 R² .. 0.999 R²)
mean                 41.66 ms   (39.13 ms .. 45.88 ms)
std dev              8.794 ms   (1.127 ms .. 13.78 ms)
variance introduced by outliers: 72% (severely inflated)

benchmarking postgresSql/pgSelectMultipleRows: {numQueries: 20, numRows: 500}
time                 800.4 ms   (771.8 ms .. 828.6 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 780.1 ms   (765.9 ms .. 787.0 ms)
std dev              13.76 ms   (2.312 ms .. 18.53 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}
time                 444.7 ms   (312.7 ms .. 468.6 ms)
                     0.986 R²   (0.953 R² .. 1.000 R²)
mean                 483.9 ms   (462.4 ms .. 517.3 ms)
std dev              35.17 ms   (888.8 μs .. 41.93 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}
time                 9.375 s    (9.124 s .. 10.03 s)
                     0.999 R²   (NaN R² .. 1.000 R²)
mean                 9.755 s    (9.540 s .. 9.855 s)
std dev              235.3 ms   (196.0 ms .. 248.9 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectRowsInParallel: {numQueries: 10}
time                 37.33 ms   (20.09 ms .. 47.17 ms)
                     0.820 R²   (0.218 R² .. 1.000 R²)
mean                 65.39 ms   (44.10 ms .. 107.4 ms)
std dev              78.39 ms   (792.8 μs .. 124.9 ms)
variance introduced by outliers: 93% (severely inflated)

benchmarking postgresSql/pgSelectRowsInParallel: {numQueries: 100}
time                 53.03 ms   (46.28 ms .. 77.04 ms)
                     0.745 R²   (0.386 R² .. 1.000 R²)
mean                 59.41 ms   (51.96 ms .. 63.55 ms)
std dev              17.13 ms   (994.3 μs .. 20.87 ms)
variance introduced by outliers: 82% (severely inflated)

benchmarking postgresSql/pgSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}
time                 66.82 ms   (58.52 ms .. 71.25 ms)
                     0.931 R²   (0.865 R² .. 0.979 R²)
mean                 65.02 ms   (59.08 ms .. 71.62 ms)
std dev              16.77 ms   (10.43 ms .. 23.48 ms)
variance introduced by outliers: 80% (severely inflated)

benchmarking postgresSql/pgSelectMultipleRowsInParallel: {numQueries: 100, numRows: 500}
time                 1.722 s    (-1.239 s .. 5.132 s)
                     0.681 R²   (0.132 R² .. 1.000 R²)
mean                 852.4 ms   (245.6 ms .. 1.291 s)
std dev              661.9 ms   (659.1 ms .. 661.9 ms)
variance introduced by outliers: 75% (severely inflated)

benchmarking pgAdapter/spannerPgSelectRowsSequentially: {numQueries: 1}
time                 46.11 ms   (43.09 ms .. 47.01 ms)
                     0.996 R²   (0.992 R² .. 1.000 R²)
mean                 48.82 ms   (47.70 ms .. 49.99 ms)
std dev              2.862 ms   (1.354 ms .. 3.683 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking pgAdapter/spannerPgSelectAndUpdateRowsInParallel: {numTransactions : 1, numRowsPerTx: 5}
time                 710.8 ms   (515.1 ms .. 1.109 s)
                     0.965 R²   (0.937 R² .. 1.000 R²)
mean                 589.1 ms   (553.7 ms .. 655.1 ms)
std dev              66.44 ms   (6.746 ms .. 78.83 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking pgAdapter/spannerPgSelectAndUpdateRowsInParallel: {numTransactions: 100, numRowsPerTx: 5}
time                 682.5 ms   (566.5 ms .. 898.7 ms)
                     0.981 R²   (0.981 R² .. 1.000 R²)
mean                 727.2 ms   (664.8 ms .. 788.7 ms)
std dev              87.81 ms   (1.952 ms .. 106.4 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking pgAdapter/spannerPgSelectRowsSequentially: {numQueries: 100}
time                 4.795 s    (4.598 s .. 5.133 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.913 s    (4.804 s .. 5.031 s)
std dev              180.6 ms   (9.050 ms .. 220.1 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking pgAdapter/spannerPgSelectMultipleRows: {numQueries: 1, numRows: 500}
time                 51.45 ms   (49.39 ms .. 53.99 ms)
                     0.987 R²   (0.939 R² .. 0.999 R²)
mean                 52.50 ms   (51.38 ms .. 53.63 ms)
std dev              3.771 ms   (1.921 ms .. 5.021 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking pgAdapter/spannerPgSelectMultipleRows: {numQueries: 20, numRows: 500}
time                 1.059 s    (994.4 ms .. 1.116 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.028 s    (1.017 s .. 1.045 s)
std dev              16.66 ms   (8.808 ms .. 23.34 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking pgAdapter/spannerPgSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}
time                 536.3 ms   (441.5 ms .. 575.6 ms)
                     0.997 R²   (0.989 R² .. 1.000 R²)
mean                 550.2 ms   (542.3 ms .. 562.0 ms)
std dev              12.69 ms   (7.591 ms .. 17.94 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking pgAdapter/spannerPgSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}
time                 11.01 s    (10.80 s .. 11.19 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 11.38 s    (11.19 s .. 11.56 s)
std dev              212.8 ms   (188.1 ms .. 217.0 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking pgAdapter/spannerPgSelectRowsInParallel: {numQueries: 10}
time                 50.01 ms   (49.18 ms .. 51.21 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 49.41 ms   (49.04 ms .. 50.50 ms)
std dev              998.0 μs   (640.9 μs .. 1.416 ms)

benchmarking pgAdapter/spannerPgSelectRowsInParallel: {numQueries: 100}
time                 71.96 ms   (59.81 ms .. 84.00 ms)
                     0.930 R²   (0.699 R² .. 0.995 R²)
mean                 73.50 ms   (69.44 ms .. 80.77 ms)
std dev              11.27 ms   (9.876 ms .. 14.58 ms)
variance introduced by outliers: 53% (severely inflated)

benchmarking pgAdapter/spannerPgSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}
time                 51.34 ms   (50.39 ms .. 52.45 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 49.78 ms   (49.28 ms .. 50.80 ms)
std dev              1.080 ms   (865.1 μs .. 1.277 ms)

benchmarking pgAdapter/spannerPgSelectMultipleRowsInParallel: {numQueries: 100, numRows: 500}
time                 107.3 ms   (91.31 ms .. 122.6 ms)
                     0.996 R²   (0.996 R² .. 1.000 R²)
mean                 111.0 ms   (108.5 ms .. 114.7 ms)
std dev              6.822 ms   (1.997 ms .. 7.427 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectRowsSequentially: {numQueries: 1}
time                 50.29 ms   (49.66 ms .. 51.64 ms)
                     0.998 R²   (0.994 R² .. 0.999 R²)
mean                 50.26 ms   (49.63 ms .. 50.73 ms)
std dev              1.231 ms   (974.3 μs .. 1.431 ms)

benchmarking googleSql/spannerSelectAndUpdateRowsInParallel: {numTransactions: 1, numRowsPerTx: 5}
time                 723.8 ms   (656.3 ms .. 851.8 ms)
                     0.996 R²   (0.992 R² .. 1.000 R²)
mean                 636.1 ms   (594.1 ms .. 679.9 ms)
std dev              46.82 ms   (4.614 ms .. 66.05 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking googleSql/spannerSelectAndUpdateRowsInParallel: {numTransactions: 100, numRowsPerTx: 5}
time                 1.532 s    (500.4 ms .. 2.062 s)
                     0.950 R²   (0.843 R² .. 1.000 R²)
mean                 1.378 s    (1.139 s .. 1.548 s)
std dev              288.2 ms   (110.0 ms .. 391.5 ms)
variance introduced by outliers: 48% (moderately inflated)

benchmarking googleSql/spannerSelectRowsSequentially: {numQueries: 100}
time                 4.105 s    (3.347 s .. 4.248 s)
                     0.994 R²   (0.981 R² .. 1.000 R²)
mean                 4.357 s    (4.248 s .. 4.551 s)
std dev              207.9 ms   (0.0 s .. 219.1 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectMultipleRows: {numQueries: 1, numRows: 500}
time                 52.63 ms   (51.43 ms .. 54.12 ms)
                     0.996 R²   (0.990 R² .. 0.999 R²)
mean                 53.53 ms   (52.78 ms .. 54.28 ms)
std dev              1.768 ms   (1.422 ms .. 2.019 ms)

benchmarking googleSql/spannerSelectMultipleRows: {numQueries: 20, numRows: 500}
time                 1.028 s    (960.5 ms .. 1.069 s)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 1.064 s    (1.044 s .. 1.083 s)
std dev              28.11 ms   (7.971 ms .. 34.19 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}
time                 547.6 ms   (428.9 ms .. 658.7 ms)
                     0.994 R²   (0.978 R² .. 1.000 R²)
mean                 528.9 ms   (513.1 ms .. 540.9 ms)
std dev              23.75 ms   (2.059 ms .. 29.07 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}
time                 10.03 s    (9.932 s .. 10.11 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.54 s    (10.32 s .. 10.67 s)
std dev              289.6 ms   (0.0 s .. 378.7 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectRowsInParallel: {numQueries: 10}
time                 76.11 ms   (47.08 ms .. 107.7 ms)
                     0.870 R²   (0.784 R² .. 0.985 R²)
mean                 71.25 ms   (68.91 ms .. 75.81 ms)
std dev              14.03 ms   (11.68 ms .. 15.33 ms)
variance introduced by outliers: 62% (severely inflated)

benchmarking googleSql/spannerSelectRowsInParallel: {numQueries: 100}
time                 3.432 s    (345.9 ms .. 10.57 s)
                     0.653 R²   (0.503 R² .. 1.000 R²)
mean                 1.023 s    (378.6 ms .. 1.660 s)
std dev              1.271 s    (11.96 ms .. 1.472 s)
variance introduced by outliers: 75% (severely inflated)

benchmarking googleSql/spannerSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}
time                 56.99 ms   (55.12 ms .. 58.61 ms)
                     0.992 R²   (0.956 R² .. 0.999 R²)
mean                 56.94 ms   (55.93 ms .. 58.16 ms)
std dev              2.578 ms   (1.458 ms .. 3.656 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking googleSql/spannerSelectMultipleRowsInParallel: {numQueries: 100, numRows: 500}
time                 322.7 ms   (304.4 ms .. 365.8 ms)
                     0.998 R²   (0.998 R² .. 1.000 R²)
mean                 505.0 ms   (434.2 ms .. 595.6 ms)
std dev              110.8 ms   (16.72 ms .. 135.2 ms)
variance introduced by outliers: 48% (moderately inflated)

