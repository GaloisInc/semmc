{-# LANGUAGE TypeApplications #-}
module Formula ( formulaTests ) where

import qualified Data.Set as Set
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified SemMC.Formula as F
import           SemMC.ToyExample ( Toy )

formulaTests :: T.TestTree
formulaTests = T.testGroup "Formula" [ sanityChecks ]

sanityChecks :: T.TestTree
sanityChecks = T.testCase "sanityChecks" $ do
  T.assertEqual "empty formula has no inputs" (F.formInputs @Toy @Int F.emptyFormula) Set.empty
  T.assertEqual "empty formula has no outputs" (F.formInputs @Toy @Int F.emptyFormula) Set.empty
