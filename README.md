# Instance Configuration
- Region: asia-south1 (Mumbai)
- Compute capacity:
    - Processing units: 100

# Results
- Spanner Instance CPU utilization (mean): 12.07%


  |   | postgresSql (pgAdapter) | googleSql
-- | -- | -- | --
selectRowsSequentially | {numQueries: 1} | 41.89 ms | 38.12 ms
  | {numQueries: 100} | 4.210 s | 4.382 s
selectMultipleRows | {numQueries: 1, numRows: 500} | 55.63 ms | 62.44 ms
  | {numQueries: 20, numRows: 500} | 1.179 s | 1.264 s
selectAndUpdateRows | {numTransactions: 1, numRowsPerTx: 5} | 592.4 ms | 630.9 ms
  | {numTransactions: 20, numRowsPerTx: 5} | 11.97 s | 12.62 s
selectRowsInParallel | {numQueries: 10} | 70.23 ms | 89.23 ms
  | {numQueries: 100} | 116.7 ms | 712.2 ms
selectMultipleRowsInParallel | {numQueries: 1, numRows: 500} | 77.60 ms | 50.51 ms
  | {numQueries: 100, numRows: 500} | 506.9 ms | 752.8 ms
selectAndUpdateRowsInParallel | {numTransactions : 1, numRowsPerTx: 5} | 671.8 ms | 561.3 ms
  | {numTransactions: 100, numRowsPerTx: 5} | 8.430 s | 7.894 s
