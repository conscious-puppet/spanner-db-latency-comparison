# Instance Configuration
- Region: asia-south1 (Mumbai)
- Compute capacity:
    - Processing units: 100

# Results
- Spanner Instance CPU utilization (mean): 12.07%
```
benchmarking postgresSql/pgSelectRowsSequentially: {numQueries: 1}
time                 49.67 ms   (49.21 ms .. 50.87 ms)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 47.80 ms   (47.48 ms .. 48.66 ms)
std dev              2.053 ms   (1.557 ms .. 2.396 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking postgresSql/pgSelectRowsSequentially: {numQueries: 100}
time                 4.983 s    (4.574 s .. 5.202 s)
                     0.997 R²   (0.997 R² .. 1.000 R²)
mean                 4.670 s    (4.670 s .. 4.670 s)
std dev              161.9 ms   (30.98 ms .. 178.9 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectMultipleRows: {numQueries: 1, numRows: 500}
time                 59.08 ms   (57.10 ms .. 61.42 ms)
                     0.992 R²   (0.985 R² .. 0.995 R²)
mean                 57.86 ms   (57.49 ms .. 58.03 ms)
std dev              2.839 ms   (2.391 ms .. 3.360 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking postgresSql/pgSelectMultipleRows: {numQueries: 20, numRows: 500}
time                 1.201 s    (1.186 s .. NaN s)
                     0.998 R²   (0.997 R² .. NaN R²)
mean                 1.157 s    (1.131 s .. 1.174 s)
std dev              43.64 ms   (43.64 ms .. 43.64 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}
time                 517.4 ms   (493.9 ms .. 530.5 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 497.5 ms   (494.8 ms .. 505.6 ms)
std dev              9.717 ms   (9.494 ms .. 10.43 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}
time                 9.789 s    (9.278 s .. 9.757 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.972 s    (9.933 s .. 10.03 s)
std dev              97.94 ms   (34.11 ms .. 119.0 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectRowsInParallel: {numQueries: 10}
time                 45.82 ms   (45.84 ms .. 46.55 ms)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 47.25 ms   (47.03 ms .. 47.94 ms)
std dev              3.312 ms   (3.207 ms .. 4.494 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking postgresSql/pgSelectRowsInParallel: {numQueries: 100}
time                 58.01 ms   (53.44 ms .. 58.07 ms)
                     0.977 R²   (0.962 R² .. 0.985 R²)
mean                 64.92 ms   (62.68 ms .. 66.96 ms)
std dev              10.11 ms   (4.053 ms .. 12.81 ms)
variance introduced by outliers: 57% (severely inflated)

benchmarking postgresSql/pgSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}
time                 50.12 ms   (49.15 ms .. 50.81 ms)
                     0.996 R²   (0.995 R² .. 0.997 R²)
mean                 52.03 ms   (51.73 ms .. 52.13 ms)
std dev              1.984 ms   (1.806 ms .. 1.989 ms)

benchmarking postgresSql/pgSelectMultipleRowsInParallel: {numQueries: 100, numRows: 500}
time                 247.5 ms   (236.3 ms .. 305.3 ms)
                     0.973 R²   (0.996 R² .. 0.998 R²)
mean                 242.4 ms   (229.9 ms .. 247.1 ms)
std dev              24.99 ms   (15.92 ms .. 25.86 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRowsInParallel: {numTransactions : 1, numRowsPerTx: 5}
time                 423.0 ms   (407.8 ms .. 423.0 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 428.3 ms   (426.1 ms .. 429.2 ms)
std dev              3.451 ms   (3.432 ms .. 4.098 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking postgresSql/pgSelectAndUpdateRowsInParallel: {numTransactions: 100, numRowsPerTx: 5}
time                 8.226 s    (7.489 s .. 8.363 s)
                     0.996 R²   (0.995 R² .. 1.000 R²)
mean                 8.285 s    (8.285 s .. 8.285 s)
std dev              320.6 ms   (154.3 ms .. 383.0 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectRowsSequentially: {numQueries: 1}
time                 41.43 ms   (41.26 ms .. 41.47 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 41.63 ms   (41.52 ms .. 41.73 ms)
std dev              585.0 μs   (503.6 μs .. 679.4 μs)

benchmarking googleSql/spannerSelectRowsSequentially: {numQueries: 100}
time                 5.931 s    (4.232 s .. 6.988 s)
                     0.948 R²   (0.948 R² .. 1.000 R²)
mean                 4.616 s    (4.256 s .. 5.335 s)
std dev              701.7 ms   (661.5 ms .. 798.9 ms)
variance introduced by outliers: 46% (moderately inflated)

benchmarking googleSql/spannerSelectMultipleRows: {numQueries: 1, numRows: 500}
time                 53.60 ms   (52.42 ms .. 53.78 ms)
                     0.994 R²   (0.986 R² .. 0.999 R²)
mean                 53.53 ms   (52.91 ms .. 54.63 ms)
std dev              2.123 ms   (1.646 ms .. 2.600 ms)

benchmarking googleSql/spannerSelectMultipleRows: {numQueries: 20, numRows: 500}
time                 1.105 s    (1.082 s .. 1.106 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.062 s    (1.058 s .. 1.066 s)
std dev              30.19 ms   (27.01 ms .. 36.18 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}
time                 1.376 s    (531.2 ms .. 2.073 s)
                     0.799 R²   (0.893 R² .. 1.000 R²)
mean                 701.6 ms   (522.3 ms .. 876.0 ms)
std dev              354.0 ms   (353.1 ms .. 411.7 ms)
variance introduced by outliers: 74% (severely inflated)

benchmarking googleSql/spannerSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}
time                 13.25 s    (10.86 s .. 14.67 s)
                     0.979 R²   (0.970 R² .. 1.000 R²)
mean                 11.12 s    (11.03 s .. 12.24 s)
std dev              1.078 s    (969.5 ms .. 1.124 s)
variance introduced by outliers: 22% (moderately inflated)

benchmarking googleSql/spannerSelectRowsInParallel: {numQueries: 10}
time                 68.21 ms   (65.91 ms .. 70.37 ms)
                     0.996 R²   (0.993 R² .. 0.997 R²)
mean                 77.13 ms   (76.98 ms .. 79.38 ms)
std dev              8.593 ms   (4.823 ms .. 12.10 ms)
variance introduced by outliers: 35% (moderately inflated)

benchmarking googleSql/spannerSelectRowsInParallel: {numQueries: 100}
time                 469.8 ms   (422.0 ms .. 509.6 ms)
                     0.993 R²   (0.987 R² .. 1.000 R²)
mean                 599.6 ms   (545.3 ms .. 606.4 ms)
std dev              99.09 ms   (18.55 ms .. 117.9 ms)
variance introduced by outliers: 46% (moderately inflated)

benchmarking googleSql/spannerSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}
time                 83.28 ms   (81.57 ms .. 98.93 ms)
                     0.983 R²   (0.976 R² .. 0.990 R²)
mean                 63.83 ms   (63.14 ms .. 66.76 ms)
std dev              12.39 ms   (11.37 ms .. 13.71 ms)
variance introduced by outliers: 62% (severely inflated)

benchmarking googleSql/spannerSelectMultipleRowsInParallel: {numQueries: 100, numRows: 500}
time                 543.8 ms   (477.6 ms .. 560.1 ms)
                     0.985 R²   (0.985 R² .. 0.993 R²)
mean                 670.5 ms   (613.5 ms .. 733.4 ms)
std dev              118.3 ms   (11.71 ms .. 131.4 ms)
variance introduced by outliers: 47% (moderately inflated)

benchmarking googleSql/spannerSelectAndUpdateRowsInParallel: {numTransactions: 1, numRowsPerTx: 5}
time                 471.4 ms   (469.3 ms .. 474.9 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 464.6 ms   (463.5 ms .. 466.5 ms)
std dev              4.819 ms   (3.573 ms .. 4.878 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking googleSql/spannerSelectAndUpdateRowsInParallel: {numTransactions: 100, numRowsPerTx: 5}
time                 7.614 s    (7.563 s .. 8.159 s)
                     0.996 R²   (0.986 R² .. 1.000 R²)
mean                 7.570 s    (7.381 s .. 7.774 s)
std dev              344.5 ms   (344.5 ms .. 344.5 ms)
variance introduced by outliers: 19% (moderately inflated)

```
