{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.Hspec.HedgehogSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Core.Spec
import           Test.Hspec.Core.Util (formatException, stripAnsi)

import           Data.List
import           Control.Exception

import qualified Test.HUnit.Lang as HUnit
import           Test.QuickCheck (stdArgs, replay)
import           Test.QuickCheck.Random (mkQCGen)

import           Test.Hspec.Hedgehog hiding (eval)

deriving instance Eq Result
deriving instance Eq ResultStatus
deriving instance Eq FailureReason

instance Eq SomeException where
  (==) = exceptionEq

exceptionEq :: SomeException -> SomeException -> Bool
exceptionEq a b
  | Just ea <- fromException a, Just eb <- fromException b = ea == (eb :: ErrorCall)
  | Just ea <- fromException a, Just eb <- fromException b = ea == (eb :: ArithException)
  | otherwise = throw (HUnit.HUnitFailure Nothing $ HUnit.ExpectedButGot Nothing (formatException b) (formatException a))

progressCallback :: ProgressCallback
progressCallback _ = return ()

params :: Params
params = defaultParams {paramsQuickCheckArgs = stdArgs {replay = Just (mkQCGen 23, 0)}}

joinLines :: [String] -> String
joinLines = intercalate "\n"

failingProperty :: PropertyT IO ()
failingProperty = failure

failingPropertyLine :: Int
failingPropertyLine = __LINE__ - 3

spec :: Spec
spec = do
  describe "evaluateExample" $ do
    let
      eval :: PropertyT IO () -> IO Result
      eval p = evaluateExample p params ($ ()) progressCallback

    context "on Success" $ do
      it "includes the number of passed tests" $ do
        eval success `shouldReturn` Result
            "  ✓ property passed 100 tests."
            Success

      it "includes classification" $ do
        eval (label "foo" >> success) `shouldReturn` Result (joinLines [
            "  ✓ property passed 100 tests."
          , "    foo 100% ████████████████████"
          ]) Success

    context "on Failure" $ do
      it "includes the number of discarded tests" $ do
        eval discard `shouldReturn` Result "" (Failure Nothing (Reason
            "  ⚐ property gave up after 10 discards, passed 0 tests."
          ))

      it "provides a detailed failure message" $ do
        Result "" (Failure (Just _loc) (ColorizedReason reason)) <- eval failingProperty
        let line delta = "    " <> show (failingPropertyLine + delta)
        stripAnsi reason `shouldBe` joinLines [
            "  ✗ property failed at test/Test/Hspec/HedgehogSpec.hs:" <> show failingPropertyLine <> ":19"
          , "    after 1 test."
          , "    shrink path: 1:"
          , "  "
          ,      "       ┏━━ test/Test/Hspec/HedgehogSpec.hs ━━━"
          , line -1 <> " ┃ failingProperty :: PropertyT IO ()"
          , line  0 <> " ┃ failingProperty = failure"
          ,      "       ┃ ^^^^^^^^^^^^^^^^^^^^^^^^^"
          , "  "
          , "    This failure can be reproduced by running:"
          , "    > recheckAt (Seed 14375056955115587481 16778118630780010967) \"1:\" property"
          , "  "
          ]
