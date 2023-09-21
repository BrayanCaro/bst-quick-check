import Test.Hspec

import qualified Examples.ListSpec
import qualified BST.Validity.BadGeneratorSpec
import qualified BST.Validity.CorrectGeneratorSpec

main :: IO ()
main = hspec $ do
  describe "Examples.ListSpec" Examples.ListSpec.spec
  describe "BST.Validity.Bad" BST.Validity.BadGeneratorSpec.spec
  describe "BST.Validity.Correct" BST.Validity.CorrectGeneratorSpec.spec


