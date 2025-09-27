module Types.Types (typesTests) where

import Test.Tasty
import Test.Tasty.HUnit
import System.Random (mkStdGen)
import Lens.Micro.Platform

import Tetrafall.Types
import Tetrafall.Types.Grid

typesTests :: TestTree  
typesTests = testGroup "Types Tests" []