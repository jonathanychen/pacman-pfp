module Main where

import Test.Hspec
import Types
import GameExecution
import qualified Data.Vector as V

main :: IO ()
main = hspec $ do
    describe "Game Logic" $ do
        it "should apply actions correctly" $ do
            let pos = (5, 5)
            applyAction pos MoveUp `shouldBe` (5, 4)
            applyAction pos MoveDown `shouldBe` (5, 6)
            applyAction pos MoveLeft `shouldBe` (4, 5)
            applyAction pos MoveRight `shouldBe` (6, 5)
        
        it "should detect walls correctly" $ do
            let grid = V.fromList [V.fromList [Wall, Empty], V.fromList [Empty, Wall]]
            isValidPosition grid (0, 0) `shouldBe` False
            isValidPosition grid (1, 0) `shouldBe` True
            isValidPosition grid (0, 1) `shouldBe` True
            isValidPosition grid (1, 1) `shouldBe` False