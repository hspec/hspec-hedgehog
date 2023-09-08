import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef             (atomicModifyIORef', readIORef, newIORef)
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Hspec             (before, beforeAll, describe, hspec, it, shouldBe)
import           Test.Hspec.Hedgehog    (PropertyT, diff, forAll, hedgehog,
                                         (/==), (===))
import           Test.Hspec.QuickCheck  (modifyMaxSuccess)

main :: IO ()
main = hspec $ do
    describe "regular tests" $ do
        it "works" $ do
            True `shouldBe` True

    describe "hedgehog" $ do
        it "is useful if you get an ambiguous error" $ hedgehog $ do
            "no ambiguity" === "no ambiguity"

    describe "hedgehog tests" $ do
        it "lets you use PropertyT directly" $ hedgehog $ do
            x <- forAll $ Gen.integral (Range.linear 0 1000)
            y <- forAll $ Gen.integral (Range.linear 0 5000)
            diff (x + y) (>=) (x :: Integer)
            
        it "lets you use PropertyT directly without forcing the type" $ do
            x <- forAll $ Gen.integral (Range.linear 0 1000)
            y <- forAll $ Gen.integral (Range.linear 0 5000)
            diff (x + y) (>=) (x :: Integer)

        it "renders a progress bit" $ hedgehog $ do
            x <- forAll $ Gen.integral (Range.linear 0 1000)
            y <- forAll $ Gen.integral (Range.linear 1 5000)
            liftIO $ threadDelay (100 * x + y)

    describe "with hooks" $ do
        before (pure "Hello!") $ do
            it "has functions" $ \str -> hedgehog $
                str === "Hello!"

            it "goes before or after" $ \str -> do
                pure () :: PropertyT IO ()
                str === "Hello!"

            it "generates" $ \str -> hedgehog $ do
                wrongLen <- forAll $ Gen.integral (Range.linear 0 3)
                length str /== wrongLen

    describe "modifyMaxSuccess" $ do
        modifyMaxSuccess (\_ -> 10) $ do
            beforeAll (newIORef (0 :: Integer)) $ do
                it "counts to 10" $ \ref -> hedgehog $ do
                    liftIO $ atomicModifyIORef' ref (\a -> (a + 1, ()))
                    True === True
                it "works" $ \ref -> do
                    val <- readIORef ref
                    val `shouldBe` 10
