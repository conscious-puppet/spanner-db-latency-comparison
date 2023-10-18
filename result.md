benchmarking postgresSql/pgSelectRowsSequentially: {numQueries: 1}
time                 42.96 ms   (41.50 ms .. 45.28 ms)
                     0.996 R²   (0.993 R² .. 0.999 R²)
mean                 41.89 ms   (41.59 ms .. 42.31 ms)
std dev              1.144 ms   (762.5 μs .. 1.449 ms)

benchmarking postgresSql/pgSelectRowsSequentially: {numQueries: 100}
time                 4.292 s    (4.008 s .. 4.525 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.210 s    (4.132 s .. 4.291 s)
std dev              98.08 ms   (46.07 ms .. 129.5 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectMultipleRows: {numQueries: 1, numRows: 500}
time                 60.25 ms   (58.54 ms .. 62.44 ms)
                     0.992 R²   (0.980 R² .. 0.998 R²)
mean                 55.63 ms   (53.86 ms .. 58.13 ms)
std dev              5.724 ms   (2.984 ms .. 8.067 ms)
variance introduced by outliers: 32% (moderately inflated)

benchmarking postgresSql/pgSelectMultipleRows: {numQueries: 20, numRows: 500}
time                 1.217 s    (1.149 s .. 1.256 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.179 s    (1.164 s .. 1.190 s)
std dev              20.26 ms   (15.45 ms .. 22.83 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}
time                 589.0 ms   (584.3 ms .. 594.7 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 592.4 ms   (590.5 ms .. 594.6 ms)
std dev              2.532 ms   (825.6 μs .. 3.068 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}
time                 12.09 s    (11.83 s .. 12.18 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.97 s    (11.94 s .. 12.01 s)
std dev              67.35 ms   (7.415 ms .. 86.15 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectRowsInParallel: {numQueries: 10}
time                 66.93 ms   (65.34 ms .. 69.52 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 70.23 ms   (68.58 ms .. 72.15 ms)
std dev              4.077 ms   (1.734 ms .. 6.031 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking postgresSql/pgSelectRowsInParallel: {numQueries: 100}
time                 110.2 ms   (101.5 ms .. 137.4 ms)
                     0.965 R²   (0.890 R² .. 0.985 R²)
mean                 116.7 ms   (101.6 ms .. 146.5 ms)
std dev              27.51 ms   (8.750 ms .. 40.64 ms)
variance introduced by outliers: 73% (severely inflated)

benchmarking postgresSql/pgSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}
time                 78.44 ms   (77.30 ms .. 82.23 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 77.60 ms   (75.38 ms .. 81.62 ms)
std dev              4.943 ms   (2.168 ms .. 7.814 ms)
variance introduced by outliers: 18% (moderately inflated)

benchmarking postgresSql/pgSelectMultipleRowsInParallel: {numQueries: 100, numRows: 500}
time                 517.5 ms   (447.3 ms .. 663.3 ms)
                     0.990 R²   (0.984 R² .. 1.000 R²)
mean                 506.9 ms   (474.9 ms .. 546.7 ms)
std dev              45.81 ms   (9.494 ms .. 62.85 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRowsInParallel: {numTransactions : 1, numRowsPerTx: 5}
time                 688.2 ms   (625.7 ms .. 763.4 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 671.8 ms   (665.3 ms .. 680.2 ms)
std dev              12.94 ms   (10.23 ms .. 14.87 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRowsInParallel: {numTransactions: 100, numRowsPerTx: 5}
time                 8.609 s    (7.897 s .. 8.731 s)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 8.430 s    (8.319 s .. 8.541 s)
std dev              141.2 ms   (72.93 ms .. 157.0 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectRowsSequentially: {numQueries: 1}
time                 37.49 ms   (35.74 ms .. 39.59 ms)
                     0.995 R²   (0.992 R² .. 0.998 R²)
mean                 38.12 ms   (37.57 ms .. 38.90 ms)
std dev              1.484 ms   (1.000 ms .. 1.913 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking googleSql/spannerSelectRowsSequentially: {numQueries: 100}
time                 5.735 s    (5.349 s .. 6.471 s)
                     0.987 R²   (0.952 R² .. 1.000 R²)
mean                 4.382 s    (4.041 s .. 4.968 s)
std dev              735.9 ms   (681.6 ms .. 737.0 ms)
variance introduced by outliers: 47% (moderately inflated)

benchmarking googleSql/spannerSelectMultipleRows: {numQueries: 1, numRows: 500}
time                 63.48 ms   (61.51 ms .. 66.92 ms)
                     0.996 R²   (0.993 R² .. 0.999 R²)
mean                 62.44 ms   (61.74 ms .. 63.58 ms)
std dev              2.772 ms   (2.167 ms .. 3.162 ms)

benchmarking googleSql/spannerSelectMultipleRows: {numQueries: 20, numRows: 500}
time                 1.276 s    (1.238 s .. 1.373 s)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.264 s    (1.255 s .. 1.285 s)
std dev              20.41 ms   (15.37 ms .. 22.42 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}
time                 637.6 ms   (612.9 ms .. 648.5 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 630.9 ms   (624.6 ms .. 636.1 ms)
std dev              6.226 ms   (3.905 ms .. 7.247 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}
time                 13.88 s    (10.32 s .. 15.51 s)
                     0.986 R²   (0.984 R² .. 1.000 R²)
mean                 12.62 s    (12.12 s .. 13.46 s)
std dev              869.2 ms   (345.8 ms .. 1.179 s)
variance introduced by outliers: 20% (moderately inflated)

benchmarking googleSql/spannerSelectRowsInParallel: {numQueries: 10}
time                 65.92 ms   (33.82 ms .. 104.0 ms)
                     0.740 R²   (0.526 R² .. 0.994 R²)
mean                 89.23 ms   (75.45 ms .. 106.7 ms)
std dev              44.09 ms   (8.737 ms .. 56.59 ms)
variance introduced by outliers: 89% (severely inflated)

benchmarking googleSql/spannerSelectRowsInParallel: {numQueries: 100}
time                 527.8 ms   (317.8 ms .. 799.6 ms)
                     0.971 R²   (0.915 R² .. 1.000 R²)
mean                 712.2 ms   (630.5 ms .. 808.4 ms)
std dev              150.9 ms   (33.39 ms .. 183.3 ms)
variance introduced by outliers: 48% (moderately inflated)

benchmarking googleSql/spannerSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}
time                 49.30 ms   (48.14 ms .. 53.16 ms)
                     0.982 R²   (0.953 R² .. 0.997 R²)
mean                 50.51 ms   (48.67 ms .. 52.74 ms)
std dev              4.286 ms   (2.893 ms .. 5.535 ms)
variance introduced by outliers: 29% (moderately inflated)

benchmarking googleSql/spannerSelectMultipleRowsInParallel: {numQueries: 100, numRows: 500}
time                 1.405 s    (633.7 ms .. 3.193 s)
                     0.833 R²   (0.746 R² .. 1.000 R²)
mean                 752.8 ms   (579.6 ms .. 1.095 s)
std dev              336.0 ms   (16.37 ms .. 400.0 ms)
variance introduced by outliers: 74% (severely inflated)

benchmarking googleSql/spannerSelectAndUpdateRowsInParallel: {numTransactions: 1, numRowsPerTx: 5}
time                 545.8 ms   (511.3 ms .. 562.4 ms)
                     1.000 R²   (0.998 R² .. 1.000 R²)
mean                 561.3 ms   (550.6 ms .. 569.0 ms)
std dev              11.03 ms   (3.262 ms .. 12.56 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectAndUpdateRowsInParallel: {numTransactions: 100, numRowsPerTx: 5}
time                 8.149 s    (7.901 s .. 8.644 s)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 7.894 s    (7.518 s .. 8.009 s)
std dev              273.1 ms   (111.3 ms .. 327.1 ms)
variance introduced by outliers: 19% (moderately inflated)

