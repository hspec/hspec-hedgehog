# Changelog for hspec-hedgehog

## 0.3.0.0

- [#45](https://github.com/hspec/hspec-hedgehog/pull/45) @ChickenProp
    - Set `propertyDiscardLimit` correctly.
- [#44](https://github.com/hspec/hspec-hedgehog/pull/44) @ChickenProp
    - Don't re-export `modifyMaxSize`, which is a no-op.

## 0.2.0.0
 - [#29](https://github.com/parsonsmatt/hspec-hedgehog/pull/29) @sol
    - Show less context on failure.

## 0.1.1.0
- [#30](https://github.com/parsonsmatt/hspec-hedgehog/pull/30) @sol
    - Show classification on success
    - Provide more information on "gave up"

## 0.1.0.0

- [#25](https://github.com/parsonsmatt/hspec-hedgehog/pull/25) @sol
    - Suppress internal source locations.
- [#24](https://github.com/parsonsmatt/hspec-hedgehog/pull/24) @sol
    - Regard `--color` / `--no-color`.
- [#23](https://github.com/parsonsmatt/hspec-hedgehog/pull/23) @ocharles
    - Improve type inference by constraining the base monad of `PropertyT` to
      `IO`.  This makes explicitly using `hedgehog` redundant in many
      situations.

## 0.0.1.2

- [#7](https://github.com/parsonsmatt/hspec-hedgehog/pull/7) @parsonsmatt
    - Handle error states better by returning them in the instance rather than throwing an exception.
- [#6](https://github.com/parsonsmatt/hspec-hedgehog/pull/6) @jezen
    - Bump lower bound on `hedgehog` to fix the build constraints.

## 0.0.1.1

- [#2](https://github.com/parsonsmatt/hspec-hedgehog/pull/2) @lehins
    - Documentation fix
- [#3](https://github.com/parsonsmatt/hspec-hedgehog/pull/3) @parsonsmatt
    - Respect the `maxSuccess`, `maxDiscardRatio`, and `maxShrinks` properties of QuickCheck's `Args` type.
    - Reexport `modifyArgs`, `modifyMaxSuccess`, `modifyMaxDiscardRatio`, `modifyMaxSize`, `modifyMaxShrings`

## 0.0.1.0

- Initial Release
