{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Conduit (MonadTrans (lift))
import Control.Concurrent.Async
import Control.Lens
import Control.Monad (forM)
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
import qualified Network.Google as Google
import qualified Network.Google.Spanner as Google
import System.Environment
import System.IO (stdout)
import System.Random

data Env = Env
  { sessionName :: T.Text,
    googleEnv :: Google.Env '["https://www.googleapis.com/auth/spanner.data"],
    sqlConn :: Pool Connection
  }

type ReaderIO a = ReaderT Env IO a

getSqlConnection :: ReaderIO (Pool Connection)
getSqlConnection = do
  Env {..} <- ask
  return sqlConn

getSessionName :: ReaderIO T.Text
getSessionName = do
  Env {..} <- ask
  return sessionName

getGoogleEnv :: ReaderIO (Google.Env '["https://www.googleapis.com/auth/spanner.data"])
getGoogleEnv = do
  Env {..} <- ask
  return googleEnv

setupEnv :: IO Env
setupEnv = do
  -- spanner db connection
  spannerSessionText <- T.pack <$> getEnv "spannerSessionText"
  lgr <- Google.newLogger Google.Debug stdout
  googleEnv <-
    Google.newEnv
      <&> (Google.envLogger .~ lgr)
        . (Google.envScopes .~ Google.spannerDataScope)
  let sessionRequest = Google.createSessionRequest
  let projectSession = Google.projectsInstancesDatabasesSessionsCreate spannerSessionText sessionRequest
  session <- runResourceT . Google.runGoogle googleEnv $ Google.send projectSession
  let sessionName = fromJust $ session ^. Google.sName

  -- Pg connection
  dbHost <- getEnv "dbHost"
  dbPort <- getEnv "dbPort"
  dbName <- getEnv "dbName"
  dbUser <- getEnv "dbUser"
  dbPassword <- getEnv "dbPassword"
  let connectionInfo =
        ConnectInfo
          { connectUser = dbUser,
            connectPort = read dbPort,
            connectPassword = dbPassword,
            connectHost = dbHost,
            connectDatabase = dbName
          }
  pool <- createPool (connect connectionInfo) close 10 20 10
  -- createPool create free numStripes idleTime maxResources = newPool PoolConfig
  --   { createResource   = create
  --   , freeResource     = free
  --   , poolCacheTTL     = realToFrac idleTime
  --   , poolMaxResources = numStripes * maxResources
  --   , poolNumStripes   = Just numStripes
  --   }
  return $ Env {sessionName = sessionName, googleEnv = googleEnv, sqlConn = pool}

myConfig :: Config
myConfig = defaultConfig {resamples = 25}

main :: IO ()
main = do
  envVariables <- setupEnv
  defaultMainWith
    myConfig
    [ bgroup
        "spanner-db-googleSql"
        [ bench "spannerSelectRowsSequentially: {numQueries: 1}" $ whnf (runReaderT (spannerSelectRowsSequentially 1)) envVariables,
          bench "spannerSelectRowsSequentially: {numQueries: 100}" $ whnf (runReaderT (spannerSelectRowsSequentially 100)) envVariables,
          bench "spannerSelectMultipleRows: {numQueries: 1, numRows: 500}" $ whnf (runReaderT (spannerSelectMultipleRows 1 500)) envVariables,
          bench "spannerSelectMultipleRows: {numQueries: 20, numRows: 500}" $ whnf (runReaderT (spannerSelectMultipleRows 20 500)) envVariables,
          bench "spannerSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}" $ whnf (runReaderT (spannerSelectAndUpdateRows 1 5)) envVariables,
          bench "spannerSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}" $ whnf (runReaderT (spannerSelectAndUpdateRows 20 5)) envVariables,
          bench "spannerSelectRowsInParallel: {numQueries: 10}" $ whnf (runReaderT (spannerSelectRowsInParallel 10)) envVariables,
          bench "spannerSelectRowsInParallel: {numQueries: 1000}" $ whnf (runReaderT (spannerSelectRowsInParallel 1000)) envVariables,
          bench "spannerSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}" $ whnf (runReaderT (spannerSelectMultipleRowsInParallel 1 500)) envVariables,
          bench "spannerSelectMultipleRowsInParallel: {numQueries: 200, numRows: 500}" $ whnf (runReaderT (spannerSelectMultipleRowsInParallel 200 500)) envVariables,
          bench "spannerSelectAndUpdateRowsInParallel: {numQueries: 1, numRows: 5}" $ whnf (runReaderT (spannerSelectAndUpdateRowsInParallel 1 5)) envVariables,
          bench "spannerSelectAndUpdateRowsInParallel: {numQueries: 200, numRows: 5}" $ whnf (runReaderT (spannerSelectAndUpdateRowsInParallel 200 5)) envVariables
        ],
      bgroup
        "spanner-db-postgresSql"
        [ bench "pgSelectRowsSequentially: {numQueries: 1}" $ whnf (runReaderT (pgSelectRowsSequentially 1)) envVariables,
          bench "pgSelectRowsSequentially: {numQueries: 100}" $ whnf (runReaderT (pgSelectRowsSequentially 100)) envVariables,
          bench "pgSelectMultipleRows: {numQueries: 1, numRows: 500}" $ whnf (runReaderT (pgSelectMultipleRows 1 500)) envVariables,
          bench "pgSelectMultipleRows: {numQueries: 20, numRows: 500}" $ whnf (runReaderT (pgSelectMultipleRows 20 500)) envVariables,
          bench "pgSelectAndUpdateRows: {numTransactions: 1, numRowsPerTx: 5}" $ whnf (runReaderT (pgSelectAndUpdateRows 1 5)) envVariables,
          bench "pgSelectAndUpdateRows: {numTransactions: 20, numRowsPerTx: 5}" $ whnf (runReaderT (pgSelectAndUpdateRows 20 5)) envVariables,
          bench "pgSelectRowsInParallel: {numQueries: 10}" $ whnf (runReaderT (pgSelectRowsInParallel 10)) envVariables,
          bench "pgSelectRowsInParallel: {numQueries: 1000}" $ whnf (runReaderT (pgSelectRowsInParallel 1000)) envVariables,
          bench "pgSelectMultipleRowsInParallel: {numQueries: 1, numRows: 500}" $ whnf (runReaderT (pgSelectMultipleRowsInParallel 1 500)) envVariables,
          bench "pgSelectMultipleRowsInParallel: {numQueries: 200, numRows: 500}" $ whnf (runReaderT (pgSelectMultipleRowsInParallel 200 500)) envVariables,
          bench "pgSelectAndUpdateRowsInParallel: {numQueries: 1, numRows: 5}" $ whnf (runReaderT (pgSelectAndUpdateRowsInParallel 1 5)) envVariables,
          bench "pgSelectAndUpdateRowsInParallel: {numQueries: 200, numRows: 5}" $ whnf (runReaderT (pgSelectAndUpdateRowsInParallel 200 5)) envVariables
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

drawInt :: Int -> IO Int
drawInt a = randomRIO (1, a)

runPgSelectOne :: ReaderIO [(Int, Int, String, String)]
runPgSelectOne = do
  sqlConn <- getSqlConnection
  randomNumber <- lift $ drawInt 1000
  let selectQuery = "select * from books where id = " <> show randomNumber
  lift $ withResource sqlConn $ \conn -> query_ conn (fromString selectQuery)

runPgSelectMany :: Int -> ReaderIO [(Int, Int, String, String)]
runPgSelectMany numRows = do
  sqlConn <- getSqlConnection
  randomNumber <- lift $ drawInt 1000
  let selectQuery = "select * from books where id > " <> show randomNumber <> " limit " <> show numRows
  lift $ withResource sqlConn $ \conn -> query_ conn (fromString selectQuery)

runPgSelectAndUpdate :: Int -> ReaderIO [[(Int, Int, String, String)]]
runPgSelectAndUpdate numRowsPerTx = do
  sqlConn <- getSqlConnection
  lift $ withResource sqlConn $ \conn -> do
    begin conn
    result <- forM [1 .. numRowsPerTx] (const $ selectAndUpdate conn)
    commit conn
    return result
  where
    selectAndUpdate conn = do
      randomIdNumber <- drawInt 1000
      let selectQuery = "select * from books where id = " <> show randomIdNumber
      result <- query_ conn (fromString selectQuery) :: IO [(Int, Int, String, String)]
      -- TODO: check if result was present

      randomNumber <- drawInt 1000
      let updateQuery = "update books set number = " <> show randomNumber <> " where id = " <> show randomIdNumber <> " returning *"

      query_ conn (fromString updateQuery)

runSpannerSelectOne :: ReaderIO [[Value]]
runSpannerSelectOne = do
  randomNumber <- lift $ drawInt 1000
  let selectQuery = T.pack $ "select * from books where id = " <> show randomNumber
  sessionName <- getSessionName
  googleEnv <- getGoogleEnv
  let e = Google.executeSQLRequest & Google.esqlrSQL ?~ selectQuery
  let p = Google.projectsInstancesDatabasesSessionsExecuteSQL e sessionName
  resultSet' <- runResourceT . Google.runGoogle googleEnv $ Google.send p
  return (resultSet' ^. Google.rsRows)

runSpannerSelectMany :: Int -> ReaderIO [[Value]]
runSpannerSelectMany numRows = do
  randomNumber <- lift $ drawInt 1000
  let selectQuery = T.pack $ "select * from books where id > " <> show randomNumber <> " limit " <> show numRows
  sessionName <- getSessionName
  googleEnv <- getGoogleEnv
  let e = Google.executeSQLRequest & Google.esqlrSQL ?~ selectQuery
  let p = Google.projectsInstancesDatabasesSessionsExecuteSQL e sessionName
  resultSet' <- runResourceT . Google.runGoogle googleEnv $ Google.send p
  return (resultSet' ^. Google.rsRows)

runSpannerSelectAndUpdate :: Int -> ReaderIO [[[Value]]]
runSpannerSelectAndUpdate numRowsPerTx = do
  sessionName <- getSessionName
  googleEnv <- getGoogleEnv

  -- begin transaction
  let txnOptions = Google.transactionOptions & Google.toReadWrite ?~ Google.readWrite
  let txnRequest = Google.beginTransactionRequest & Google.btrOptions ?~ txnOptions
  let projectTxnRequest = Google.projectsInstancesDatabasesSessionsBeginTransaction txnRequest sessionName
  transaction <- runResourceT . Google.runGoogle googleEnv $ Google.send projectTxnRequest
  let transactionId = fromJust (transaction ^. Google.tId)

  -- execute the queries
  resultSet' <- forM [1 .. numRowsPerTx] (selectAndUpdate transactionId)

  -- commit this transaction
  let commitRequest = Google.commitRequest & Google.crTransactionId ?~ transactionId
  let projectCommitRequest = Google.projectsInstancesDatabasesSessionsCommit commitRequest sessionName
  commitResponse' <- runResourceT . Google.runGoogle googleEnv $ Google.send projectCommitRequest
  let _ = fromJust (commitResponse' ^. Google.crCommitTimestamp)

  return $ map (^. Google.rsRows) resultSet'
  where
    selectAndUpdate transactionId seqNo = do
      let txnSelector = Google.transactionSelector & Google.tsId ?~ transactionId

      sessionName <- getSessionName
      googleEnv <- getGoogleEnv

      randomIdNumber <- lift $ drawInt 1000
      let selectQuery' = T.pack $ "select * from books where id = " <> show randomIdNumber
      let selectQuery =
            Google.executeSQLRequest
              & Google.esqlrSQL ?~ selectQuery'
      let executeSelectQuery = Google.projectsInstancesDatabasesSessionsExecuteSQL selectQuery sessionName
      resultSet' <- runResourceT . Google.runGoogle googleEnv $ Google.send executeSelectQuery
      -- TODO: check if resultSet' was present

      randomNumber <- lift $ drawInt 1000
      -- let updateQuery' = T.pack $ "update books set number = " <> show randomNumber <> " where id = " <> show randomIdNumber <> " then return id, number, name, author"
      let updateQuery' = T.pack $ "update books set number = " <> show randomNumber <> " where id = " <> show randomIdNumber

      let updateQuery =
            Google.executeSQLRequest
              & Google.esqlrSQL ?~ updateQuery'
              & Google.esqlrTransaction ?~ txnSelector
              & Google.esqlrSeqno ?~ fromIntegral seqNo
      let executeUpdateQuery = Google.projectsInstancesDatabasesSessionsExecuteSQL updateQuery sessionName
      runResourceT . Google.runGoogle googleEnv $ Google.send executeUpdateQuery
