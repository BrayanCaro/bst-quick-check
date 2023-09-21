import Test.Hspec

import qualified Examples.ListSpec
import qualified BST.Validity.BadGeneratorSpec
import qualified BST.Validity.CorrectGeneratorSpec
import qualified BST.Postconditions.PostSpec
import qualified BST.Metamorphic.BadSpec
import qualified BST.Metamorphic.CorrectSpec
import qualified BST.Inductive.CorrectSpec
import qualified BST.ModelBased.BadSpec
import qualified BST.ModelBased.CorrectSpec

main :: IO ()
main = hspec $ do
  describe "Examples.ListSpec" Examples.ListSpec.spec
  describe "BST.Validity.Bad" BST.Validity.BadGeneratorSpec.spec
  describe "BST.Validity.Correct" BST.Validity.CorrectGeneratorSpec.spec
  describe "BST.Postconditions.Post" BST.Postconditions.PostSpec.spec
  describe "BST.Metamorphic.Bad" BST.Metamorphic.BadSpec.spec
  describe "BST.Metamorphic.Correct" BST.Metamorphic.CorrectSpec.spec
  describe "BST.Inductive.Correct" BST.Inductive.CorrectSpec.spec
  describe "BST.ModelBased.Bad" BST.ModelBased.BadSpec.spec
  describe "BST.ModelBased.Correct" BST.ModelBased.CorrectSpec.spec



