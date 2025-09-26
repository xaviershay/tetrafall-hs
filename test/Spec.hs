import Test.Tasty
import Test.Tasty.HUnit

import Types.Grid (gridTests)
import Types.Types (typesTests)
import Types.Slide (slideTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tetrafall Tests"
  [ unitTests
  , gridTests
  , typesTests
  , slideTests
  ]

unitTests :: TestTree
unitTests = testGroup "Example Tests"
  [ testGroup "Basic assertions"
    [ testCase "Equality test" $ 
        2 + 2 @?= (4 :: Int)
    
    , testCase "Boolean assertion" $ 
        assertBool "Should be true" True
    
    , testCase "String equality" $ 
        "hello" @?= "hello"
    
    , testCase "List equality" $ 
        [1 :: Int, 2, 3] @?= [1, 2, 3]
    ]
  
  , testGroup "Property-style tests"
    [ testCase "Addition is commutative" $ do
        let a = 5 :: Int
        let b = 3
        a + b @?= b + a
    
    , testCase "List append properties" $ do
        let xs = [1 :: Int, 2]
        let ys = [3, 4]
        length (xs ++ ys) @?= length xs + length ys
    ]
  
  , testGroup "Exception and error handling"
    [ testCase "Division by zero (should not crash in test)" $ do
        let result = if True then 42 else (5 `div` 0)
        result @?= (42 :: Int)
    
    , testCase "Empty list operations" $ do
        assertBool "Empty list is null" (null [])
        length [] @?= (0 :: Int)
    ]
  
  , testGroup "Multi-step tests"
    [ testCaseSteps "Step-by-step calculation" $ \step -> do
        step "Setting up initial values"
        let x = 10 :: Int
        let y = 5
        
        step "Performing addition"
        let sum_result = x + y
        sum_result @?= 15
        
        step "Performing multiplication" 
        let mult_result = x * y
        mult_result @?= 50
        
        step "Final validation"
        assertBool "Sum should be less than product" (sum_result < mult_result)
    ]
  
  , testGroup "Testing with IO"
    [ testCase "IO operations" $ do
        -- Testing pure values wrapped in IO
        result <- return (10 * 5 :: Int)
        result @?= 50
        
        -- Testing boolean conditions with IO
        condition <- return True
        assertBool "IO condition should be true" condition
    ]
  ]


