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

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
--   'it' \"message\" $ \\str -> do
--     'pure' () :: 'PropertyT' 'IO' ()
--     wrongLen <- 'forAll' $ 'Gen.integral' ('Range.linear' 0 3)
--     'length' str '/==' wrongLen
-- @
--
-- You don't have to remember to put the 'hedgehog' call after the lambda.
module Test.Hspec.Hedgehog
    ( -- * The Main Function
      hedgehog
      -- * Hspec re-exports
    , modifyArgs
    , modifyMaxSuccess
    , modifyMaxDiscardRatio
    , modifyMaxSize
    , modifyMaxShrinks
      -- * Hedgehog Re-exports
    , module Hedgehog
    ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Coerce                (coerce)
import           Data.IORef                 (newIORef, readIORef, writeIORef)
import           GHC.Stack                  (withFrozenCallStack)
import           Hedgehog
import           Hedgehog.Internal.Config   (detectColor)
import           Hedgehog.Internal.Property (DiscardLimit (..), Property (..),
                                             PropertyConfig (..),
                                             ShrinkLimit (..),
                                             TerminationCriteria (..),
                                             TestCount (..), TestLimit (..))
import           Hedgehog.Internal.Report   as Hedge
import           Hedgehog.Internal.Runner   (checkReport)
import qualified Hedgehog.Internal.Seed     as Seed
import           Hedgehog.Internal.Source   (ColumnNo (..), LineNo (..),
                                             Span (..))
import           System.Random.SplitMix     (unseedSMGen)
import           Test.Hspec
import           Test.Hspec.Core.Spec       as Hspec
import           Test.Hspec.QuickCheck      (modifyArgs, modifyMaxDiscardRatio,
                                             modifyMaxShrinks, modifyMaxSize,
                                             modifyMaxSuccess)
import           Test.QuickCheck.Random     (QCGen (..))
import           Test.QuickCheck.Test       (Args (..))

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
instance m ~ IO => Example (PropertyT m ()) where
    type Arg (PropertyT m ()) = ()
    evaluateExample e = evaluateExample (\() -> e)

propertyWithoutCallStack :: PropertyT IO () -> Property
propertyWithoutCallStack = withFrozenCallStack property

-- | Warning: orphan instance! This instance is used to embed a "Hedgehog"
-- property seamlessly into the @hspec@ framework.
--
-- The instance will pick things up from the "Test.Hspec.QuickCheck"
-- configuration. For example, if the program is supposed to use
-- a predetermined seed, then the same seed will be used for QuickCheck and
-- Hedgehog tests.
--
-- @since 0.0.0.0
instance (m ~ IO) => Example (a -> PropertyT m ()) where
    type Arg (a -> PropertyT m ()) = a

    evaluateExample (fmap propertyWithoutCallStack -> aprop) params aroundAction progressCallback = do
        ref <- newIORef (Result "" (Pending Nothing Nothing))
        aroundAction $ \a ->  do
            color <- detectColor
            let size = 0
                prop = aprop a
                propConfig = useQuickCheckArgs (propertyConfig prop)
                qcArgs = paramsQuickCheckArgs params

                maxTests = maxSuccess qcArgs
                useQuickCheckArgs pc =
                    pc
                    { propertyTerminationCriteria =
                        case propertyTerminationCriteria pc of
                            EarlyTermination x (TestLimit _)      ->
                                EarlyTermination x (TestLimit maxTests)
                            NoEarlyTermination x (TestLimit _)    ->
                                NoEarlyTermination x (TestLimit maxTests)
                            NoConfidenceTermination (TestLimit _) ->
                                NoConfidenceTermination (TestLimit maxTests)
                    , propertyDiscardLimit =
                        DiscardLimit $ maxDiscardRatio qcArgs
                    , propertyShrinkLimit =
                        ShrinkLimit $ maxShrinks qcArgs
                    }
                testCount report =
                    case reportTests report of
                        TestCount n -> n
                cb progress = do
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
            writeIORef ref $ Result "" $ case reportStatus hedgeResult of
                Failed FailureReport{..} ->
                    let
                        fromSpan Span{..} =
                            Location
                                { locationFile = spanFile
                                , locationLine = coerce spanStartLine
                                , locationColumn = coerce spanStartColumn
                                }
                    in
                        Hspec.Failure (fromSpan <$> failureLocation) $ Reason ppresult
                GaveUp ->
                    Failure Nothing (Reason "GaveUp")
                OK ->
                    Success
        readIORef ref
