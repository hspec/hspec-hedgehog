{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | This module allows you to easily integrate the "Hedgehog" library with
-- "Test.Hspec" test-suites.
--
-- To get started, check out the 'hedgehog' function, which lets you embed
-- a 'PropertyT' directly.
--
-- @
-- spec :: 'Spec'
-- spec =
--   'describe' \"my great test\" '$' do
--     'it' \"generates stuff\" '$'
--       'hedgehog' '$' do
--         a <- 'forAll' generator
--         a '===' expected
-- @
--
-- Truth be told, the functionality is in the two orphan instances of
-- 'Example' for 'PropertyT'. You can directly use code in the @'PropertyT'
-- 'IO'@ type. However, because most "Hedgehog" functions are abstract in
-- 'MonadTest', you might get errors about ambiguous types. The 'hedgehog'
-- function fixes the type to @'PropertyT' 'IO' '()'@, which works out just
-- fine.
--
-- You can use all of @hspec@'s hooks with this, of course.
--
-- @
-- spec :: Spec
-- spec = 'before' ('pure' \"Hello!\") '$' do
--   'describe' \"with a string\" '$' do
--     'it' \"gets a string\" '$' \\ str ->
--       'hedgehog' '$' do
--         wrongLen <- 'forAll' $ 'Gen.integral' ('Range.linear' 0 3)
--         length str '/==' wrongLen
-- @
--
-- The function 'before' will make all the following spec items a function,
-- accepting that as a parameter. You should call 'hedgehog' after the
-- lambda.
--
-- If you are morally opposed to the pattern:
--
-- @
-- 'it' \"message\" $ 'hedgehog' $ do
--   True '===' False
-- @
--
-- Then you can alternatively force the type some other way. One option is
-- to use a no-op function, like this:
--
-- @
-- 'it' \"message\" $ do
--   'pure' () :: 'PropertyT' 'IO' ()
--   True '===' False
-- @
--
-- This style has the advantage that parameters via hooks are less
-- difficult to get right.
--
-- @
-- 'before' ('pure' \"Hello!\") $ do
--   'it' \"message\" $ \str -> do
--     'pure' () :: 'PropertyT' 'IO' ()
--     wrongLen <- 'forAll' $ 'Gen.integral' ('Range.linear' 0 3)
--     'length' str '/==' wrongLen
-- @
--
-- You don't have to remember to put the 'hedgehog' call after the lambda.
module Test.Hspec.Hedgehog
    ( hedgehog
    , module Hedgehog
    ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Coerce                (coerce)
import           Data.IORef                 (newIORef, readIORef, writeIORef)
import           Hedgehog
import           Hedgehog.Internal.Config   (UseColor, detectColor)
import           Hedgehog.Internal.Property (TerminationCriteria (..),
                                             TestCount (..), TestLimit (..),
                                             propertyConfig,
                                             propertyTerminationCriteria,
                                             propertyTest)
import           Hedgehog.Internal.Report   as Hedge
import           Hedgehog.Internal.Runner   (checkReport)
import qualified Hedgehog.Internal.Seed     as Seed
import           Hedgehog.Internal.Source   (HasCallStack)
import           System.Random.SplitMix     (unseedSMGen)
import           Test.Hspec
import           Test.Hspec.Core.Spec       as Hspec
import           Test.HUnit.Base            (assertFailure)
import           Test.QuickCheck.Random     (QCGen (..))
import           Test.QuickCheck.Test       (replay)

-- | Embed a "Hedgehog" @'PropertyT' 'IO' ()@ in an @hspec@ test.
--
-- @
-- spec :: 'Spec'
-- spec =
--   'describe' \"my great test\" '$' do
--     'it' \"generates stuff\" '$'
--       'hedgehog' '$' do
--         a <- 'forAll' generator
--         a '===' expected
-- @
--
-- This function is only used to fix the type of the @'PropertyT'@ monad
-- transformer. The functions in "Hedgehog" are typically abstract in
-- a 'MonadTest', and it's easy to get ambiguous type errors if you leave
-- this out.
--
-- @since 0.0.0.0
hedgehog :: HasCallStack => PropertyT IO () -> PropertyT IO ()
hedgehog = id

-- |  Warning: Orphan instance! This instance is used to embed a "Hedgehog"
-- property seamlessly into the @hspec@ framework. See the other instance
-- of 'Example' for a function for more details.
--
-- @since 0.0.0.0
instance Example (PropertyT IO ()) where
    type Arg (PropertyT IO ()) = ()
    evaluateExample e = evaluateExample (\() -> e)

-- | Warning: orphan instance! This instance is used to embed a "Hedgehog"
-- property seamlessly into the @hspec@ framework.
--
-- The instance will pick things up from the "Test.Hspec.QuickCheck"
-- configuration. For example, if the program is supposed to use
-- a predetermined seed, then the same seed will be used for QuickCheck and
-- Hedgehog tests.
--
-- @since 0.0.0.0
instance Example (a -> PropertyT IO ()) where
    type Arg (a -> PropertyT IO ()) = a

    evaluateExample (fmap property -> aprop) params aroundAction progressCallback = do
        ref <- newIORef (Result "" (Pending Nothing Nothing))
        aroundAction $ \a ->  do
            color <- detectColor
            let size = 0
                prop = aprop a
                propConfig = propertyConfig prop
                maxTests = case propertyTerminationCriteria propConfig of
                    EarlyTermination _ (TestLimit n)      -> n
                    NoEarlyTermination _ (TestLimit n)    -> n
                    NoConfidenceTermination (TestLimit n) -> n
                testCount report = case reportTests report of
                    TestCount n -> n
                cb progress = do
                    ppprogress <- renderProgress color Nothing progress
                    case reportStatus progress of
                        Running ->
                            progressCallback (testCount progress, maxTests)
                        Shrinking _ ->
                            progressCallback (testCount progress, maxTests)

            seed <- liftIO $ case replay (paramsQuickCheckArgs params) of
               Nothing       -> Seed.random
               Just (rng, _) -> pure (uncurry Seed (unseedSMGen (coerce rng)))
            hedgeResult <- checkReport propConfig size seed (propertyTest prop) cb
            ppresult <- renderResult color Nothing hedgeResult
            writeIORef ref =<< case reportStatus hedgeResult of
              Failed FailureReport{..} -> do
    --              let fromSpan Span{..} =
    --                    Location
    --                        { locationFile = spanFile
    --                        , locationLine = coerce spanStartLine
    --                        , locationColumn = coerce spanStartColumn
    --                        }
    --              pure
    --                $ Result ""
    --                $ Hspec.Failure (fromSpan <$> failureLocation) _h -- ppresult
                  assertFailure ppresult
              GaveUp ->
                  assertFailure ppresult
              OK ->
                  pure $ Result "" Success
        readIORef ref
      where
        mkFail = Result
