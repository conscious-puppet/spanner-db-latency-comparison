{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (
  main,
)
where

import Conduit (MonadTrans (lift))
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad (forM, void)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource (runResourceT)
import Criterion.Main
import Criterion.Types
import Data.Aeson
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.Pool as Pool
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Transaction
import qualified Network.Google as Google
import qualified Network.Google.Spanner as Google
import System.Environment
import System.IO (stdout)
import System.Random

data Env = Env
  { spannerSessionPool :: Pool T.Text
  , googleEnv :: Google.Env '["https://www.googleapis.com/auth/spanner.data"]
  , spannerSqlConnPool :: Pool Connection
  , sqlConnPool :: Pool Connection
  }

type ReaderIO a = ReaderT Env IO a

getSqlConnPool :: ReaderIO (Pool Connection)
getSqlConnPool = do
  Env{..} <- ask
  return sqlConnPool

createSession :: T.Text -> Google.Env '["https://www.googleapis.com/auth/spanner.data"] -> IO T.Text
createSession spannerSessionText googleEnv = do
  let sessionRequest = Google.createSessionRequest
  let projectSession = Google.projectsInstancesDatabasesSessionsCreate spannerSessionText sessionRequest
  session <- runResourceT . Google.runGoogle googleEnv $ Google.send projectSession
  let sessionName = fromJust $ session ^. Google.sName
  return sessionName

deleteSession :: Google.Env '["https://www.googleapis.com/auth/spanner.data"] -> T.Text -> IO ()
deleteSession googleEnv sessionName = do
  let deleteProjectSession = Google.projectsInstancesDatabasesSessionsDelete sessionName
  void $ runResourceT . Google.runGoogle googleEnv $ Google.send deleteProjectSession

getGoogleEnv :: ReaderIO (Google.Env '["https://www.googleapis.com/auth/spanner.data"])
getGoogleEnv = do
  Env{..} <- ask
  return googleEnv

getSpannerSessionPool :: ReaderIO (Pool T.Text)
getSpannerSessionPool = do
  Env{..} <- ask
  return spannerSessionPool

setupEnv :: IO Env
setupEnv = do
  -- spanner db connection
  spannerSessionText <- T.pack <$> getEnv "spannerSessionText"
  lgr <- Google.newLogger Google.Error stdout
  googleEnv <-
    Google.newEnv
      <&> (Google.envLogger .~ lgr)
        . (Google.envScopes .~ Google.spannerDataScope)
  spannerSessionPool <- createPool (createSession spannerSessionText googleEnv) (deleteSession googleEnv) 10 2000 100

  -- spanner pgsql interface connection
  spannerDbHost <- getEnv "spannerDbHost"
  spannerDbPort <- getEnv "spannerDbPort"
  spannerDbName <- getEnv "spannerDbName"
  spannerDbUser <- getEnv "spannerDbUser"
  spannerDbPassword <- getEnv "spannerDbPassword"
  let spannerConnectionInfo =
        ConnectInfo
          { connectUser = spannerDbUser
          , connectPort = read spannerDbPort
          , connectPassword = spannerDbPassword
          , connectHost = spannerDbHost
          , connectDatabase = spannerDbName
          }
  spannerSqlConnPool <- createPool (connect spannerConnectionInfo) close 10 2000 100

  -- pgsql connection
  dbHost <- getEnv "dbHost"
  dbPort <- getEnv "dbPort"
  dbName <- getEnv "dbName"
  dbUser <- getEnv "dbUser"
  dbPassword <- getEnv "dbPassword"
  let connectionInfo =
        ConnectInfo
          { connectUser = dbUser
          , connectPort = read dbPort
          , connectPassword = dbPassword
          , connectHost = dbHost
          , connectDatabase = dbName
          }
  sqlConnPool <- createPool (connect connectionInfo) close 10 2000 100
  -- createPool create free numStripes idleTime maxResources = newPool PoolConfig
  --   { createResource   = create
  --   , freeResource     = free
  --   , poolCacheTTL     = realToFrac idleTime
  --   , poolMaxResources = numStripes * maxResources
  --   , poolNumStripes   = Just numStripes
  --   }
  return $ Env{spannerSessionPool = spannerSessionPool, googleEnv = googleEnv, spannerSqlConnPool = spannerSqlConnPool, sqlConnPool = sqlConnPool}

myConfig :: Config
myConfig =
  defaultConfig
    { resamples = 20
    , reportFile = Just "reportFile.html"
    }

main :: IO ()
main = do
  envVariables <- setupEnv
  defaultMainWith
    myConfig
    [ bgroup
        "spanner postgresSql interface"
        [ bench "spannerPgSelectRowsSequentially: {numQueries: 1}" $ nfAppIO (runReaderT (spannerPgSelectRowsSequentially 1)) envVariables
        , bench "spannerPgSelectRowsSequentially: {numQueries: 100}" $ nfAppIO (runReaderT (spannerPgSelectRowsSequentially 100)) envVariables
        , bench "spannerPgSelectMultipleRows: {numQueries: 1, numRows: 500}" $ nfAppIO (runReaderT (spannerPgSelectMultipleRows 1 500)) envVariables
        , bench "spannerPgSelectMultipleRows: {numQueries: 20, numRows: 500}" $ nfAppIO (runReaderT (spannerPgSelectMultipleRows 20 500)) envVariables
        , bench "spannerPgSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}" $ nfAppIO (runReaderT (spannerPgSelectAndUpdateRows 1 5)) envVariables
        , bench "spannerPgSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}" $ nfAppIO (runReaderT (spannerPgSelectAndUpdateRows 20 5)) envVariables
        , bench "spannerPgSelectRowsInParallel: {numQueries: 10}" $ nfAppIO (runReaderT (spannerPgSelectRowsInParallel 10)) envVariables
        , bench "spannerPgSelectRowsInParallel: {numQueries: 100}" $ nfAppIO (runReaderT (spannerPgSelectRowsInParallel 100)) envVariables
        , bench "spannerPgSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}" $ nfAppIO (runReaderT (spannerPgSelectMultipleRowsInParallel 1 500)) envVariables
        , bench "spannerPgSelectMultipleRowsInParallel: {numQueries: 100, numRows: 500}" $ nfAppIO (runReaderT (spannerPgSelectMultipleRowsInParallel 100 500)) envVariables
        , bench "spannerPgSelectAndUpdateRowsInParallel: {numTransactions : 1, numRowsPerTx: 5}" $ nfAppIO (runReaderT (spannerPgSelectAndUpdateRowsInParallel 1 5)) envVariables
        , bench "spannerPgSelectAndUpdateRowsInParallel: {numTransactions: 100, numRowsPerTx: 5}" $ nfAppIO (runReaderT (spannerPgSelectAndUpdateRowsInParallel 100 5)) envVariables
        ]
    , bgroup
        "postgresSql"
        [ bench "pgSelectRowsSequentially: {numQueries: 1}" $ nfAppIO (runReaderT (pgSelectRowsSequentially 1)) envVariables
        , bench "pgSelectRowsSequentially: {numQueries: 100}" $ nfAppIO (runReaderT (pgSelectRowsSequentially 100)) envVariables
        , bench "pgSelectMultipleRows: {numQueries: 1, numRows: 500}" $ nfAppIO (runReaderT (pgSelectMultipleRows 1 500)) envVariables
        , bench "pgSelectMultipleRows: {numQueries: 20, numRows: 500}" $ nfAppIO (runReaderT (pgSelectMultipleRows 20 500)) envVariables
        , bench "pgSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}" $ nfAppIO (runReaderT (pgSelectAndUpdateRows 1 5)) envVariables
        , bench "pgSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}" $ nfAppIO (runReaderT (pgSelectAndUpdateRows 20 5)) envVariables
        , bench "pgSelectRowsInParallel: {numQueries: 10}" $ nfAppIO (runReaderT (pgSelectRowsInParallel 10)) envVariables
        , bench "pgSelectRowsInParallel: {numQueries: 100}" $ nfAppIO (runReaderT (pgSelectRowsInParallel 100)) envVariables
        , bench "pgSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}" $ nfAppIO (runReaderT (pgSelectMultipleRowsInParallel 1 500)) envVariables
        , bench "pgSelectMultipleRowsInParallel: {numQueries: 100, numRows: 500}" $ nfAppIO (runReaderT (pgSelectMultipleRowsInParallel 100 500)) envVariables
        , bench "pgSelectAndUpdateRowsInParallel: {numTransactions : 1, numRowsPerTx: 5}" $ nfAppIO (runReaderT (pgSelectAndUpdateRowsInParallel 1 5)) envVariables
        , bench "pgSelectAndUpdateRowsInParallel: {numTransactions: 100, numRowsPerTx: 5}" $ nfAppIO (runReaderT (pgSelectAndUpdateRowsInParallel 100 5)) envVariables
        ]
    , bgroup
        "googleSql"
        [ bench "spannerSelectRowsSequentially: {numQueries: 1}" $ nfAppIO (runReaderT (spannerSelectRowsSequentially 1)) envVariables
        , bench "spannerSelectRowsSequentially: {numQueries: 100}" $ nfAppIO (runReaderT (spannerSelectRowsSequentially 100)) envVariables
        , bench "spannerSelectMultipleRows: {numQueries: 1, numRows: 500}" $ nfAppIO (runReaderT (spannerSelectMultipleRows 1 500)) envVariables
        , bench "spannerSelectMultipleRows: {numQueries: 20, numRows: 500}" $ nfAppIO (runReaderT (spannerSelectMultipleRows 20 500)) envVariables
        , bench "spannerSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}" $ nfAppIO (runReaderT (spannerSelectAndUpdateRows 1 5)) envVariables
        , bench "spannerSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}" $ nfAppIO (runReaderT (spannerSelectAndUpdateRows 20 5)) envVariables
        , bench "spannerSelectRowsInParallel: {numQueries: 10}" $ nfAppIO (runReaderT (spannerSelectRowsInParallel 10)) envVariables
        , bench "spannerSelectRowsInParallel: {numQueries: 100}" $ nfAppIO (runReaderT (spannerSelectRowsInParallel 100)) envVariables
        , bench "spannerSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}" $ nfAppIO (runReaderT (spannerSelectMultipleRowsInParallel 1 500)) envVariables
        , bench "spannerSelectMultipleRowsInParallel: {numQueries: 100, numRows: 500}" $ nfAppIO (runReaderT (spannerSelectMultipleRowsInParallel 100 500)) envVariables
        , bench "spannerSelectAndUpdateRowsInParallel: {numTransactions: 1, numRowsPerTx: 5}" $ nfAppIO (runReaderT (spannerSelectAndUpdateRowsInParallel 1 5)) envVariables
        , bench "spannerSelectAndUpdateRowsInParallel: {numTransactions: 100, numRowsPerTx: 5}" $ nfAppIO (runReaderT (spannerSelectAndUpdateRowsInParallel 100 5)) envVariables
        ]
    ]

spannerSelectRowsSequentially :: Int -> ReaderIO ()
spannerSelectRowsSequentially numQueries = for_ [1 .. numQueries] (const runSpannerSelectOne)

spannerSelectMultipleRows :: Int -> Int -> ReaderIO ()
spannerSelectMultipleRows numQueries numRows = for_ [1 .. numQueries] (const $ runSpannerSelectMany numRows)

spannerSelectAndUpdateRows :: Int -> Int -> ReaderIO ()
spannerSelectAndUpdateRows numTransactions numRowsPerTx = for_ [1 .. numTransactions] (const $ runSpannerSelectAndUpdate numRowsPerTx)

spannerSelectRowsInParallel :: Int -> ReaderIO ()
spannerSelectRowsInParallel numQueries =
  ask >>= \env' ->
    lift $
      forConcurrently_
        [1 .. numQueries]
        (const $ runReaderT runSpannerSelectOne env')

spannerSelectMultipleRowsInParallel :: Int -> Int -> ReaderIO ()
spannerSelectMultipleRowsInParallel numQueries numRows =
  ask >>= \env' ->
    lift $
      forConcurrently_
        [1 .. numQueries]
        (const $ runReaderT (runSpannerSelectMany numRows) env')

spannerSelectAndUpdateRowsInParallel :: Int -> Int -> ReaderIO ()
spannerSelectAndUpdateRowsInParallel numTransactions numRowsPerTx =
  ask >>= \env' ->
    lift $
      forConcurrently_
        [1 .. numTransactions]
        (const $ runReaderT (runSpannerSelectAndUpdate numRowsPerTx) env')

pgSelectRowsSequentially :: Int -> ReaderIO ()
pgSelectRowsSequentially numQueries = for_ [1 .. numQueries] (const runPgSelectOne)

pgSelectMultipleRows :: Int -> Int -> ReaderIO ()
pgSelectMultipleRows numQueries numRows = for_ [1 .. numQueries] (const $ runPgSelectMany numRows)

pgSelectAndUpdateRows :: Int -> Int -> ReaderIO ()
pgSelectAndUpdateRows numTransactions numRowsPerTx = for_ [1 .. numTransactions] (const $ runPgSelectAndUpdate numRowsPerTx)

pgSelectRowsInParallel :: Int -> ReaderIO ()
pgSelectRowsInParallel numQueries =
  ask >>= \env' ->
    lift $
      forConcurrently_
        [1 .. numQueries]
        (const $ runReaderT runPgSelectOne env')

pgSelectMultipleRowsInParallel :: Int -> Int -> ReaderIO ()
pgSelectMultipleRowsInParallel numQueries numRows =
  ask >>= \env' ->
    lift $
      forConcurrently_
        [1 .. numQueries]
        (const $ runReaderT (runPgSelectMany numRows) env')

pgSelectAndUpdateRowsInParallel :: Int -> Int -> ReaderIO ()
pgSelectAndUpdateRowsInParallel numTransactions numRowsPerTx =
  ask >>= \env' ->
    lift $
      forConcurrently_
        [1 .. numTransactions]
        (const $ runReaderT (runPgSelectAndUpdate numRowsPerTx) env')

spannerPgSelectRowsSequentially :: Int -> ReaderIO ()
spannerPgSelectRowsSequentially numQueries = for_ [1 .. numQueries] (const runSpannerPgSelectOne)

spannerPgSelectMultipleRows :: Int -> Int -> ReaderIO ()
spannerPgSelectMultipleRows numQueries numRows = for_ [1 .. numQueries] (const $ runSpannerPgSelectMany numRows)

spannerPgSelectAndUpdateRows :: Int -> Int -> ReaderIO ()
spannerPgSelectAndUpdateRows numTransactions numRowsPerTx = for_ [1 .. numTransactions] (const $ runSpannerPgSelectAndUpdate numRowsPerTx)

spannerPgSelectRowsInParallel :: Int -> ReaderIO ()
spannerPgSelectRowsInParallel numQueries =
  ask >>= \env' ->
    lift $
      forConcurrently_
        [1 .. numQueries]
        (const $ runReaderT runSpannerPgSelectOne env')

spannerPgSelectMultipleRowsInParallel :: Int -> Int -> ReaderIO ()
spannerPgSelectMultipleRowsInParallel numQueries numRows =
  ask >>= \env' ->
    lift $
      forConcurrently_
        [1 .. numQueries]
        (const $ runReaderT (runSpannerPgSelectMany numRows) env')

spannerPgSelectAndUpdateRowsInParallel :: Int -> Int -> ReaderIO ()
spannerPgSelectAndUpdateRowsInParallel numTransactions numRowsPerTx =
  ask >>= \env' ->
    lift $
      forConcurrently_
        [1 .. numTransactions]
        (const $ runReaderT (runSpannerPgSelectAndUpdate numRowsPerTx) env')

drawInt :: Int -> IO Int
drawInt a = randomRIO (1, a)

runPgSelectOne :: ReaderIO [(Int, Int, String, String)]
runPgSelectOne = do
  sqlConnPool <- getSqlConnPool
  randomNumber <- lift $ drawInt 1000
  let selectQuery = "select * from books where id = " <> show randomNumber
  lift $ withResource sqlConnPool $ \conn -> tryCatch (query_ conn) (fromString selectQuery)

runPgSelectMany :: Int -> ReaderIO [(Int, Int, String, String)]
runPgSelectMany numRows = do
  sqlConnPool <- getSqlConnPool
  randomNumber <- lift $ drawInt 1000
  let selectQuery = "select * from books where id > " <> show randomNumber <> " limit " <> show numRows
  lift $ withResource sqlConnPool $ \conn -> tryCatch (query_ conn) (fromString selectQuery)

runPgSelectAndUpdate :: Int -> ReaderIO [[(Int, Int, String, String)]]
runPgSelectAndUpdate numRowsPerTx = do
  sqlConnPool <- getSqlConnPool
  env' <- ask
  lift $ withResource sqlConnPool $ \conn ->
    withTransactionMode (TransactionMode Serializable ReadWrite) conn $
      forM [1 .. numRowsPerTx] (const $ selectAndUpdate conn env')
 where
  selectAndUpdate conn env' = do
    randomIdNumber <- drawInt 1000
    sqlConnPool <- runReaderT getSqlConnPool env'
    let selectQuery = "select * from books where id = " <> show randomIdNumber
    void $ withResource sqlConnPool $ \conn' -> tryCatch (query_ conn') (fromString selectQuery) :: IO [(Int, Int, String, String)]

    randomNumber <- drawInt 1000
    let updateQuery = "update books set number = " <> show randomNumber <> " where id = " <> show randomIdNumber <> " returning *"

    tryCatch (query_ conn) (fromString updateQuery)

runSpannerPgSelectOne :: ReaderIO [(Int, Int, String, String)]
runSpannerPgSelectOne = do
  sqlConnPool <- getSqlConnPool
  randomNumber <- lift $ drawInt 1000
  let selectQuery = "select * from books where id = " <> show randomNumber
  lift $ withResource sqlConnPool $ \conn -> do
    tryCatch (query_ conn) (fromString selectQuery)

runSpannerPgSelectMany :: Int -> ReaderIO [(Int, Int, String, String)]
runSpannerPgSelectMany numRows = do
  sqlConnPool <- getSqlConnPool
  randomNumber <- lift $ drawInt 1000
  let selectQuery = "select * from books where id > " <> show randomNumber <> " limit " <> show numRows
  lift $ withResource sqlConnPool $ \conn -> tryCatch (query_ conn) (fromString selectQuery)

runSpannerPgSelectAndUpdate :: Int -> ReaderIO [[(Int, Int, String, String)]]
runSpannerPgSelectAndUpdate numRowsPerTx = do
  sqlConnPool <- getSqlConnPool
  env' <- ask
  lift $ withResource sqlConnPool $ \conn ->
    withTransactionMode (TransactionMode Serializable ReadWrite) conn $
      forM [1 .. numRowsPerTx] (const $ selectAndUpdate conn env')
 where
  selectAndUpdate conn env' = do
    randomIdNumber <- drawInt 1000
    sqlConnPool <- runReaderT getSqlConnPool env'
    let selectQuery = "select * from books where id = " <> show randomIdNumber
    void $ withResource sqlConnPool $ \conn' -> tryCatch (query_ conn') (fromString selectQuery) :: IO [(Int, Int, String, String)]

    randomNumber <- drawInt 1000
    let updateQuery = "update books set number = " <> show randomNumber <> " where id = " <> show randomIdNumber <> " returning *"

    tryCatch (query_ conn) (fromString updateQuery)

runSpannerSelectOne :: ReaderIO [[Value]]
runSpannerSelectOne = do
  randomNumber <- lift $ drawInt 1000
  let selectQuery = T.pack $ "select * from books where id = " <> show randomNumber
  sessionPool <- getSpannerSessionPool
  googleEnv <- getGoogleEnv
  lift $ withResource sessionPool $ \sessionName -> do
    let e = Google.executeSQLRequest & Google.esqlrSQL ?~ selectQuery
    let p = Google.projectsInstancesDatabasesSessionsExecuteSQL e sessionName
    resultSet' <- runResourceT . Google.runGoogle googleEnv $ Google.send p
    return (resultSet' ^. Google.rsRows)

runSpannerSelectMany :: Int -> ReaderIO [[Value]]
runSpannerSelectMany numRows = do
  randomNumber <- lift $ drawInt 1000
  let selectQuery = T.pack $ "select * from books where id > " <> show randomNumber <> " limit " <> show numRows
  sessionPool <- getSpannerSessionPool
  googleEnv <- getGoogleEnv
  lift $ withResource sessionPool $ \sessionName -> do
    let e = Google.executeSQLRequest & Google.esqlrSQL ?~ selectQuery
    let p = Google.projectsInstancesDatabasesSessionsExecuteSQL e sessionName
    resultSet' <- runResourceT . Google.runGoogle googleEnv $ Google.send p
    return (resultSet' ^. Google.rsRows)

runSpannerSelectAndUpdate :: Int -> ReaderIO [[[Value]]]
runSpannerSelectAndUpdate numRowsPerTx = do
  googleEnv <- getGoogleEnv
  sessionPool <- getSpannerSessionPool
  lift $ withResource sessionPool $ \sessionName -> do
    -- begin transaction
    let txnOptions = Google.transactionOptions & Google.toReadWrite ?~ Google.readWrite
    let txnRequest = Google.beginTransactionRequest & Google.btrOptions ?~ txnOptions
    let projectTxnRequest = Google.projectsInstancesDatabasesSessionsBeginTransaction txnRequest sessionName
    transaction <- runResourceT . Google.runGoogle googleEnv $ Google.send projectTxnRequest
    let transactionId = fromJust (transaction ^. Google.tId)

    -- execute the queries
    resultSet' <- forM [1 .. numRowsPerTx] (selectAndUpdate transactionId googleEnv sessionName)

    -- commit this transaction
    let commitRequest = Google.commitRequest & Google.crTransactionId ?~ transactionId
    let projectCommitRequest = Google.projectsInstancesDatabasesSessionsCommit commitRequest sessionName
    commitResponse' <- runResourceT . Google.runGoogle googleEnv $ Google.send projectCommitRequest
    let _ = fromJust (commitResponse' ^. Google.crCommitTimestamp)
    -- lift $ putResource localPool sessionName
    return $ map (^. Google.rsRows) resultSet'
 where
  selectAndUpdate transactionId googleEnv sessionName seqNo = do
    let txnSelector = Google.transactionSelector & Google.tsId ?~ transactionId

    randomIdNumber <- drawInt 1000
    let selectQuery' = T.pack $ "select * from books where id = " <> show randomIdNumber
    let selectQuery =
          Google.executeSQLRequest
            & Google.esqlrSQL ?~ selectQuery'
    let executeSelectQuery = Google.projectsInstancesDatabasesSessionsExecuteSQL selectQuery sessionName
    void $ runResourceT . Google.runGoogle googleEnv $ Google.send executeSelectQuery

    randomNumber <- drawInt 1000
    -- let updateQuery' = T.pack $ "update books set number = " <> show randomNumber <> " where id = " <> show randomIdNumber <> " then return id, number, name, author"
    let updateQuery' = T.pack $ "update books set number = " <> show randomNumber <> " where id = " <> show randomIdNumber

    let updateQuery =
          Google.executeSQLRequest
            & Google.esqlrSQL ?~ updateQuery'
            & Google.esqlrTransaction ?~ txnSelector
            & Google.esqlrSeqno ?~ fromIntegral seqNo
    let executeUpdateQuery = Google.projectsInstancesDatabasesSessionsExecuteSQL updateQuery sessionName
    runResourceT . Google.runGoogle googleEnv $ Google.send executeUpdateQuery

tryCatch :: forall a b. (a -> IO b) -> a -> IO b
tryCatch f input = do
  errored :: Either SomeException b <- try $ f input
  either (const $ tryCatch f input) return errored
