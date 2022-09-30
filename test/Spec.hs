import Test.Tasty
import Test.Tasty.HUnit

import Lib2 (renderDocument)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [toYaml])

toYaml :: TestTree
toYaml = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "null"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5"
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]
