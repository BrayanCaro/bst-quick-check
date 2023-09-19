import Test.Hspec

import qualified Examples.ListSpec

main :: IO ()
main = hspec $ do
  describe "Examples.ListSpec" Examples.ListSpec.spec


