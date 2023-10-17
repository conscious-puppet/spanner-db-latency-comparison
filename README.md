```
benchmarking spanner-db-googleSql/spannerSelectRowsSequentially: {numQueries: 1}
time                 4.299 ns   (4.295 ns .. 4.307 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.303 ns   (4.299 ns .. 4.313 ns)
std dev              30.53 ps   (8.620 ps .. 49.97 ps)

benchmarking spanner-db-googleSql/spannerSelectRowsSequentially: {numQueries: 100}
time                 4.331 ns   (4.318 ns .. 4.363 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.341 ns   (4.334 ns .. 4.348 ns)
std dev              29.03 ps   (25.89 ps .. 32.87 ps)

benchmarking spanner-db-googleSql/spannerSelectMultipleRows: {numQueries: 1, numRows: 500}
time                 8.503 ns   (8.500 ns .. 8.507 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.499 ns   (8.493 ns .. 8.509 ns)
std dev              25.77 ps   (18.13 ps .. 31.97 ps)

benchmarking spanner-db-googleSql/spannerSelectMultipleRows: {numQueries: 20, numRows: 500}
time                 8.519 ns   (8.502 ns .. 8.574 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.504 ns   (8.498 ns .. 8.519 ns)
std dev              41.24 ps   (10.53 ps .. 67.53 ps)

benchmarking spanner-db-googleSql/spannerSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}
time                 4.284 ns   (4.281 ns .. 4.288 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.285 ns   (4.283 ns .. 4.295 ns)
std dev              22.68 ps   (17.02 ps .. 37.30 ps)

benchmarking spanner-db-googleSql/spannerSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}
time                 4.351 ns   (4.340 ns .. 4.358 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.331 ns   (4.324 ns .. 4.340 ns)
std dev              39.30 ps   (36.54 ps .. 42.31 ps)

benchmarking spanner-db-googleSql/spannerSelectRowsInParallel: {numQueries: 10}
time                 4.297 ns   (4.294 ns .. 4.301 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.288 ns   (4.286 ns .. 4.292 ns)
std dev              14.36 ps   (12.56 ps .. 16.73 ps)

benchmarking spanner-db-googleSql/spannerSelectRowsInParallel: {numQueries: 1000}
time                 4.356 ns   (4.337 ns .. 4.370 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.318 ns   (4.308 ns .. 4.332 ns)
std dev              38.12 ps   (28.06 ps .. 46.37 ps)

benchmarking spanner-db-googleSql/spannerSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}
time                 4.390 ns   (4.387 ns .. 4.392 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.385 ns   (4.382 ns .. 4.387 ns)
std dev              7.685 ps   (6.400 ps .. 10.53 ps)

benchmarking spanner-db-googleSql/spannerSelectMultipleRowsInParallel: {numQueries: 200, numRows: 500}
time                 4.390 ns   (4.387 ns .. 4.392 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.389 ns   (4.387 ns .. 4.390 ns)
std dev              6.668 ps   (3.503 ps .. 8.797 ps)

benchmarking spanner-db-googleSql/spannerSelectAndUpdateRowsInParallel: {numQueries: 1, numRows: 5}
time                 4.385 ns   (4.384 ns .. 4.388 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.385 ns   (4.384 ns .. 4.386 ns)
std dev              4.900 ps   (3.449 ps .. 6.516 ps)

benchmarking spanner-db-googleSql/spannerSelectAndUpdateRowsInParallel: {numQueries: 200, numRows: 5}
time                 4.386 ns   (4.383 ns .. 4.389 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.386 ns   (4.384 ns .. 4.388 ns)
std dev              7.660 ps   (5.348 ps .. 10.04 ps)

benchmarking spanner-db-postgresSql/pgSelectRowsSequentially: {numQueries: 1}
time                 4.385 ns   (4.383 ns .. 4.386 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.386 ns   (4.385 ns .. 4.389 ns)
std dev              8.541 ps   (5.983 ps .. 12.64 ps)

benchmarking spanner-db-postgresSql/pgSelectRowsSequentially: {numQueries: 100}
time                 4.389 ns   (4.385 ns .. 4.392 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.386 ns   (4.384 ns .. 4.389 ns)
std dev              10.40 ps   (6.780 ps .. 14.19 ps)

benchmarking spanner-db-postgresSql/pgSelectMultipleRows: {numQueries: 1, numRows: 500}
time                 4.386 ns   (4.382 ns .. 4.390 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.383 ns   (4.382 ns .. 4.385 ns)
std dev              4.486 ps   (3.048 ps .. 7.071 ps)

benchmarking spanner-db-postgresSql/pgSelectMultipleRows: {numQueries: 20, numRows: 500}
time                 4.390 ns   (4.385 ns .. 4.399 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.392 ns   (4.386 ns .. 4.396 ns)
std dev              21.64 ps   (11.25 ps .. 27.28 ps)

benchmarking spanner-db-postgresSql/pgSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}
time                 4.384 ns   (4.382 ns .. 4.385 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.382 ns   (4.381 ns .. 4.384 ns)
std dev              4.316 ps   (2.650 ps .. 6.145 ps)

benchmarking spanner-db-postgresSql/pgSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}
time                 4.384 ns   (4.378 ns .. 4.387 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.386 ns   (4.384 ns .. 4.389 ns)
std dev              9.874 ps   (4.894 ps .. 14.26 ps)

benchmarking spanner-db-postgresSql/pgSelectRowsInParallel: {numQueries: 10}
time                 4.361 ns   (4.360 ns .. 4.366 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.360 ns   (4.359 ns .. 4.361 ns)
std dev              7.106 ps   (5.195 ps .. 9.141 ps)

benchmarking spanner-db-postgresSql/pgSelectRowsInParallel: {numQueries: 1000}
time                 4.365 ns   (4.364 ns .. 4.367 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.367 ns   (4.364 ns .. 4.370 ns)
std dev              8.282 ps   (3.757 ps .. 11.35 ps)

benchmarking spanner-db-postgresSql/pgSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}
time                 4.360 ns   (4.358 ns .. 4.362 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.360 ns   (4.359 ns .. 4.362 ns)
std dev              6.195 ps   (5.611 ps .. 7.304 ps)

benchmarking spanner-db-postgresSql/pgSelectMultipleRowsInParallel: {numQueries: 200, numRows: 500}
time                 4.369 ns   (4.364 ns .. 4.378 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.370 ns   (4.369 ns .. 4.374 ns)
std dev              12.72 ps   (10.79 ps .. 16.50 ps)

benchmarking spanner-db-postgresSql/pgSelectAndUpdateRowsInParallel: {numQueries: 1, numRows: 5}
time                 4.362 ns   (4.361 ns .. 4.364 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.360 ns   (4.360 ns .. 4.362 ns)
std dev              3.904 ps   (3.285 ps .. 5.096 ps)

benchmarking spanner-db-postgresSql/pgSelectAndUpdateRowsInParallel: {numQueries: 200, numRows: 5}
time                 4.366 ns   (4.365 ns .. 4.368 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.367 ns   (4.364 ns .. 4.375 ns)
std dev              10.98 ps   (7.403 ps .. 18.38 ps)
```
