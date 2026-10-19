{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}

module SoupTests (soupTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Labeled
import Effectful.Labeled.Reader qualified as LR
import Effectful.Reader.Static (Reader, ask)
import Effectful.Soup
import Polytropos (emptyRecord, insertField)
import Utils qualified as U

soupTests :: TestTree
soupTests = testGroup "Soup"
  [ testCase "askSoup and asksSoup fetch contexts from soup" test_askSoup
  , testCase "localSoup updates context within local scope" test_localSoup
  , testCase "filtering restricts soup to sub-row via SubRow" test_filtering
  , testCase "withSoupLabel bridges soup context to unlabelled Reader" test_withSoupLabel
  , testCase "withSoupAsLabeled bridges soup context to Labeled Reader" test_withSoupAsLabeled
  , testCase "OverloadedLabels combinators for Labeled and Labeled.Reader" test_labeledOverloadedLabels
  ]

-- | Test environment record schema
type AppEnv =
  '[ "db"     ':= String
   , "port"   ':= Int
   , "logger" ':= String
   ]

type SubEnv =
  '[ "db"   ':= String
   , "port" ':= Int
   ]

testEnv :: Record AppEnv
testEnv = REC
  { db     = "postgres://localhost:5432/test"
  , port   = 8080
  , logger = "stdout"
  }

test_askSoup :: Assertion
test_askSoup = runEff . runSoup testEnv $ do
  db <- askSoup #db
  port <- askSoup #port
  logger <- askSoup #logger
  portNext <- asksSoup #port (+ 1)
  U.assertEqual "db" "postgres://localhost:5432/test" db
  U.assertEqual "port" 8080 port
  U.assertEqual "logger" "stdout" logger
  U.assertEqual "portNext" 8081 portNext

test_localSoup :: Assertion
test_localSoup = runEff . runSoup testEnv $ do
  p0 <- askSoup #port
  p1 <- localSoup #port (* 2) $ do
    askSoup #port
  p2 <- askSoup #port
  U.assertEqual "initial port" 8080 p0
  U.assertEqual "local doubled port" 16160 p1
  U.assertEqual "restored port" 8080 p2

test_filtering :: Assertion
test_filtering = runEff . runSoup testEnv $ do
  res <- filtering @SubEnv subAction
  U.assertEqual "subAction result" "postgres://localhost:5432/test:8080" res
  where
    subAction :: Eff (Soup SubEnv : es) String
    subAction = do
      db <- askSoup #db
      port <- askSoup #port
      pure $ db ++ ":" ++ show port

test_withSoupLabel :: Assertion
test_withSoupLabel = runEff . runSoup testEnv $ do
  res <- withSoupLabel #port readerAction
  U.assertEqual "readerAction result" 8080 res
  where
    readerAction :: (Reader Int :> es) => Eff es Int
    readerAction = ask

test_withSoupAsLabeled :: Assertion
test_withSoupAsLabeled = runEff . runSoup testEnv $ do
  res <- withSoupAsLabeled #port labeledReaderAction
  U.assertEqual "labeledReaderAction result" 8080 res
  where
    labeledReaderAction :: (Labeled "port" (LR.Reader Int) :> es) => Eff es Int
    labeledReaderAction = LR.askBy #port

data Greeter :: Effect where
  Greet :: String -> Greeter m String

type instance DispatchOf Greeter = Dynamic

runGreeter :: Eff (Greeter : es) a -> Eff es a
runGreeter = interpret $ \_ -> \case
  Greet name -> pure ("Hello, " ++ name ++ "!")

test_labeledOverloadedLabels :: Assertion
test_labeledOverloadedLabels = runEff $ do
  -- Run labeled dynamic effect with runLabeledBy
  runLabeledBy #greeter runGreeter $ do
    msg <- sendLabeled #greeter (Greet "Alice")
    U.assertEqual "greeted" "Hello, Alice!" msg

  -- Run labeled Reader with runReaderBy and query via askBy / asksBy / localBy
  LR.runReaderBy #config (100 :: Int) $ do
    v0 <- LR.askBy #config
    v1 <- LR.asksBy #config (* (2 :: Int))
    v2 <- LR.localBy #config (+ (5 :: Int)) (LR.askBy #config)
    U.assertEqual "askBy" (100 :: Int) v0
    U.assertEqual "asksBy" (200 :: Int) v1
    U.assertEqual "localBy" (105 :: Int) v2
