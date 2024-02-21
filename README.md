# Results
- [Criterion Report File](https://conscious-puppet.github.io/spanner-db-latency-comparison/reportFile.html)

|                               |                                         | postgresSql | pgAdapter | googleSql |
|-------------------------------|-----------------------------------------|-------------|-----------|-----------|
| selectRowsSequentially        | {numQueries: 1}                         | 30.65 ms    | 48.82 ms  | 50.26 ms  |
|                               | {numQueries: 100}                       | 3.741 s     | 4.913 s   | 4.357 s   |
| selectMultipleRows            | {numQueries: 1, numRows: 500}           | 41.66 ms    | 52.50 ms  | 53.53 ms  |
|                               | {numQueries: 20, numRows: 500}          | 780.1 ms    | 1.028 s   | 1.064 s   |
| selectAndUpdateRows           | {numTransactions: 1, numRowsPerTx: 5}   | 483.9 ms    | 550.2 ms  | 528.9 ms  |
|                               | {numTransactions: 20, numRowsPerTx: 5}  | 9.755 s     | 11.38 s   | 10.54 s   |
| selectRowsInParallel          | {numQueries: 10}                        | 65.39 ms    | 49.41 ms  | 71.25 ms  |
|                               | {numQueries: 100}                       | 59.41 ms    | 73.50 ms  | 1.023 s   |
| selectMultipleRowsInParallel  | {numQueries: 1, numRows: 500}           | 65.02 ms    | 49.78 ms  | 56.94 ms  |
|                               | {numQueries: 100, numRows: 500}         | 852.4 ms    | 111.0 ms  | 505.0 ms  |
| selectAndUpdateRowsInParallel | {numTransactions : 1, numRowsPerTx: 5}  | 1.012 s     | 589.1 ms  | 636.1 ms  |
|                               | {numTransactions: 100, numRowsPerTx: 5} | 1.580 s     | 727.2 ms  | 1.378 s   |
