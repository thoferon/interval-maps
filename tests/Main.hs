import Control.Applicative
import Control.Monad
import Data.Function ((&))
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.IntervalMap as IvMap

main :: IO ()
main = defaultMain $ testGroup "Unit tests"
  [ testProperty "delete and insert work" $ property $ do
      ivMap <- forAll $ Gen.filter ((/= 0) . IvMap.size) intervalMapGen
      let ((interval, old) : _) = IvMap.toList ivMap
          inInterval =
            case (IvMap.lowerBound interval, IvMap.upperBound interval) of
              (IvMap.Closed lb, _) -> lb
              (_, IvMap.Closed ub) -> ub
              (IvMap.Open lb, IvMap.Open ub) -> (lb + ub) / 2
              (IvMap.Infinity, IvMap.Open ub) -> ub - 1
              (IvMap.Open lb, IvMap.Infinity) -> lb + 1
              (IvMap.Infinity, IvMap.Infinity) -> 0
      new <- forAll $ Gen.integral $ Range.linear 0 1000

      IvMap.insert interval new ivMap === Nothing
      IvMap.lookup inInterval ivMap === Just old
      (ivMap
        & ( IvMap.delete inInterval
        >=> IvMap.insert interval new
        >=> IvMap.lookup inInterval)) === Just new
  ]

intervalBoundGen :: Gen (IvMap.IntervalBound Double)
intervalBoundGen = do
  int <- Gen.double $ Range.linearFrac 1 10000
  Gen.frequency
    [ (9, pure $ IvMap.Closed int)
    , (9, pure $ IvMap.Open int)
    , (2, pure IvMap.Infinity)
    ]

intervalGen :: Gen (IvMap.Interval Double)
intervalGen = do
  bound  <- intervalBoundGen
  bound' <- intervalBoundGen
  maybe empty pure $
    IvMap.interval bound bound'
    <|>
    IvMap.interval bound' bound

intervalMapGen :: Gen (IvMap.IntervalMap Double Integer)
intervalMapGen =
  Gen.just . fmap IvMap.fromList . Gen.list (Range.linear 0 100) $
    (,)
      <$> intervalGen
      <*> Gen.integral (Range.linear 0 1000)
