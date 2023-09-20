import Test.Hspec

import qualified Examples.ListSpec
import qualified BST.Generator.BadGeneratorSpec
import qualified BST.Generator.CorrectGeneratorSpec

main :: IO ()
main = hspec $ do
  describe "Examples.ListSpec" Examples.ListSpec.spec
  describe "BST.DataSpec.Generator.Bad" BST.Generator.BadGeneratorSpec.spec
  describe "BST.DataSpec.Generator.Correct" BST.Generator.CorrectGeneratorSpec.spec


