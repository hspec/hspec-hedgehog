# Changelog for hspec-hedgehog

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
