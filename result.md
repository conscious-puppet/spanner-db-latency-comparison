benchmarking postgresSql/pgSelectRowsSequentially: {numQueries: 1}
time                 44.68 ms   (44.46 ms .. 47.61 ms)
                     0.974 R²   (0.964 R² .. 0.985 R²)
mean                 41.08 ms   (40.56 ms .. 41.36 ms)
std dev              3.219 ms   (2.959 ms .. 4.126 ms)
variance introduced by outliers: 27% (moderately inflated)

benchmarking postgresSql/pgSelectRowsSequentially: {numQueries: 100}
time                 3.766 s    (3.660 s .. 3.821 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.117 s    (4.016 s .. 4.239 s)
std dev              212.5 ms   (111.3 ms .. 259.0 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectMultipleRows: {numQueries: 1, numRows: 500}
time                 53.80 ms   (53.89 ms .. 55.91 ms)
                     0.992 R²   (0.976 R² .. 0.995 R²)
mean                 55.60 ms   (54.82 ms .. 57.91 ms)
std dev              3.943 ms   (3.351 ms .. 5.150 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking postgresSql/pgSelectMultipleRows: {numQueries: 20, numRows: 500}
time                 1.076 s    (NaN s .. 1.109 s)
                     0.999 R²   (NaN R² .. 1.000 R²)
mean                 1.139 s    (1.127 s .. 1.148 s)
std dev              35.33 ms   (24.78 ms .. 49.70 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}
time                 453.8 ms   (440.6 ms .. 468.8 ms)
                     0.999 R²   (1.000 R² .. 1.000 R²)
mean                 480.7 ms   (469.9 ms .. 480.9 ms)
std dev              24.59 ms   (2.200 ms .. 25.33 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}
time                 9.186 s    (9.104 s .. 9.259 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.270 s    (9.255 s .. 9.289 s)
std dev              44.14 ms   (44.14 ms .. 44.14 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectRowsInParallel: {numQueries: 10}
time                 38.89 ms   (37.98 ms .. 38.54 ms)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 41.35 ms   (40.93 ms .. 42.12 ms)
std dev              4.453 ms   (4.409 ms .. 6.053 ms)
variance introduced by outliers: 45% (moderately inflated)

benchmarking postgresSql/pgSelectRowsInParallel: {numQueries: 100}
time                 61.21 ms   (41.48 ms .. 46.93 ms)
                     0.719 R²   (0.814 R² .. 0.986 R²)
mean                 67.47 ms   (60.34 ms .. 74.14 ms)
std dev              17.63 ms   (14.68 ms .. 20.48 ms)
variance introduced by outliers: 82% (severely inflated)

benchmarking postgresSql/pgSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}
time                 43.83 ms   (42.56 ms .. 45.12 ms)
                     0.995 R²   (0.989 R² .. 0.999 R²)
mean                 43.92 ms   (43.55 ms .. 44.11 ms)
std dev              2.172 ms   (2.172 ms .. 2.172 ms)
variance introduced by outliers: 13% (moderately inflated)

benchmarking postgresSql/pgSelectMultipleRowsInParallel: {numQueries: 100, numRows: 500}
time                 416.6 ms   (234.5 ms .. 436.9 ms)
                     0.976 R²   (0.997 R² .. 1.000 R²)
mean                 491.9 ms   (478.3 ms .. 491.9 ms)
std dev              57.45 ms   (39.15 ms .. 61.15 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRowsInParallel: {numTransactions : 1, numRowsPerTx: 5}
time                 434.3 ms   (408.8 ms .. 454.3 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 433.9 ms   (431.7 ms .. 437.1 ms)
std dev              5.695 ms   (2.206 ms .. 7.416 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRowsInParallel: {numTransactions: 100, numRowsPerTx: 5}
time                 9.463 s    (9.205 s .. 9.572 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.391 s    (9.342 s .. 9.441 s)
std dev              81.89 ms   (32.78 ms .. 97.94 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectRowsSequentially: {numQueries: 1}
time                 36.69 ms   (35.99 ms .. 37.76 ms)
                     0.995 R²   (0.989 R² .. 1.000 R²)
mean                 37.55 ms   (36.89 ms .. 38.58 ms)
std dev              3.097 ms   (3.042 ms .. 4.082 ms)
variance introduced by outliers: 32% (moderately inflated)

benchmarking googleSql/spannerSelectRowsSequentially: {numQueries: 100}
time                 4.193 s    (4.196 s .. 4.201 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.312 s    (4.268 s .. 4.355 s)
std dev              80.87 ms   (25.79 ms .. 86.06 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectMultipleRows: {numQueries: 1, numRows: 500}
time                 53.73 ms   (50.30 ms .. 53.63 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 53.40 ms   (53.05 ms .. 53.70 ms)
std dev              1.879 ms   (1.879 ms .. 1.879 ms)

benchmarking googleSql/spannerSelectMultipleRows: {numQueries: 20, numRows: 500}
time                 1.071 s    (1.064 s .. 1.071 s)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 1.067 s    (1.063 s .. 1.069 s)
std dev              20.09 ms   (16.14 ms .. 20.56 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}
time                 561.7 ms   (553.1 ms .. 580.1 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 543.9 ms   (543.8 ms .. 550.6 ms)
std dev              8.828 ms   (8.562 ms .. 8.999 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}
time                 11.29 s    (10.88 s .. 11.39 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 10.91 s    (10.80 s .. 11.02 s)
std dev              191.8 ms   (22.41 ms .. 239.3 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectRowsInParallel: {numQueries: 10}
time                 35.32 ms   (31.36 ms .. 45.42 ms)
                     0.849 R²   (0.797 R² .. 0.987 R²)
mean                 80.60 ms   (56.81 ms .. 100.3 ms)
std dev              74.13 ms   (14.46 ms .. 100.7 ms)
variance introduced by outliers: 92% (severely inflated)

benchmarking googleSql/spannerSelectRowsInParallel: {numQueries: 100}
time                 430.7 ms   (414.5 ms .. 436.9 ms)
                     0.995 R²   (0.980 R² .. 0.999 R²)
mean                 662.2 ms   (656.6 ms .. 690.8 ms)
std dev              142.2 ms   (127.4 ms .. 155.5 ms)
variance introduced by outliers: 48% (moderately inflated)

benchmarking googleSql/spannerSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}
time                 63.29 ms   (59.32 ms .. 64.71 ms)
                     0.983 R²   (0.982 R² .. 0.997 R²)
mean                 61.37 ms   (59.80 ms .. 61.87 ms)
std dev              4.815 ms   (4.392 ms .. 5.287 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking googleSql/spannerSelectMultipleRowsInParallel: {numQueries: 100, numRows: 500}
time                 644.0 ms   (684.4 ms .. 1.081 s)
                     0.949 R²   (0.911 R² .. 1.000 R²)
mean                 609.1 ms   (541.0 ms .. 659.9 ms)
std dev              77.17 ms   (47.49 ms .. 90.63 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking googleSql/spannerSelectAndUpdateRowsInParallel: {numTransactions: 1, numRowsPerTx: 5}
time                 479.9 ms   (459.2 ms .. 497.2 ms)
                     0.998 R²   (0.998 R² .. 1.000 R²)
mean                 477.3 ms   (469.0 ms .. 488.3 ms)
std dev              10.75 ms   (8.453 ms .. 10.75 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectAndUpdateRowsInParallel: {numTransactions: 100, numRowsPerTx: 5}
time                 7.582 s    (7.262 s .. 7.685 s)
                     0.998 R²   (0.993 R² .. 1.000 R²)
mean                 7.452 s    (7.328 s .. 7.576 s)
std dev              272.3 ms   (266.0 ms .. 315.2 ms)
variance introduced by outliers: 19% (moderately inflated)

