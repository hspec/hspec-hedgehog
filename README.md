# hspec-hedgehog

An integration library for [hspec](https://hackage.haskell.org/package/hspec) and [hedgehog](https://hackage.haskell.org/package/hedgehog).

Example:

```haskell
import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class (liftIO)
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Hspec             (before, describe, hspec, it, shouldBe)
import           Test.Hspec.Hedgehog    (PropertyT, diff, forAll, hedgehog,
                                         (/==), (===))

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
            diff (x + y) (>=) x

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
```

## How does this differ from [`hw-hspec-hedgehog`](https://hackage.haskell.org/package/hw-hspec-hedgehog)?

Good question!

The `hw-spec-hedgehog` implementation does the easy thing.
It calls Hedgehog's `check` function on the property, and if the property returns `True`, then it passes the test.
If the property fails, then it renders an [uninformative failure message](https://twitter.com/mattoflambda/status/1234879820225400832) -  it's hardcoded to be:

> Hedgehog property test failed

And that's all you get!

This library preserves Hedgehog's error message formatting, so you get [rich, insightful error messages](https://twitter.com/mattoflambda/status/1234880271406661633) just like Hedgehog intended.

Furthermore, this library integrates with `hspec`'s support for the `QuickCheck` library.
Any option that works with `QuickCheck` should work with `hedgehog` properties, so you can use `modifyMaxSuccess (\_ -> 10)` to set the total tests to be 10, rather than the default 100.

Because it integrates directly with hspec, it also renders a familiar progress message while the test is running.
