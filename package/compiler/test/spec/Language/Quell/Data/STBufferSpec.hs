module Language.Quell.Data.STBufferSpec (spec) where

import           Language.Quell.Prelude
import           Test.Hspec

import qualified Language.Quell.Data.STBuffer as STBuffer


spec :: Spec
spec = do
    describe "new" do
        it "returns empty buffer with empty list" do
            buf <- stToIO do STBuffer.new @Int []
            l1 <- stToIO do STBuffer.toList buf
            l1 `shouldBe` []
            b1 <- stToIO do STBuffer.isEmpty buf
            b1 `shouldBe` True
            stToIO do STBuffer.appendLast 1 buf
            l2 <- stToIO do STBuffer.toList buf
            l2 `shouldBe` [1]
            b2 <- stToIO do STBuffer.isEmpty buf
            b2 `shouldBe` False
            mx <- stToIO do STBuffer.consumeHead buf
            mx `shouldBe` Just 1
            l3 <- stToIO do STBuffer.toList buf
            l3 `shouldBe` []
            b3 <- stToIO do STBuffer.isEmpty buf
            b3 `shouldBe` True

        it "returns buffer with big list" do
            let l0 = replicate 100 () :: [()]
            buf <- stToIO do STBuffer.new l0
            l1 <- stToIO do STBuffer.toList buf
            l1 `shouldBe` l0

    describe "length" do
        it "returns initialized list length without any operations" do
            let l0 = [(), (), (), ()]
            buf <- stToIO do STBuffer.new l0
            len <- stToIO do STBuffer.length buf
            len `shouldBe` length l0

    describe "identity" do
        it "be satisfied for consumeHead / appendHead" do
            let l0 = [0,0,0,0,0,0,0] :: [Int]
            buf <- stToIO do STBuffer.new l0
            stToIO do STBuffer.appendHead 1 buf
            l1 <- stToIO do STBuffer.toList buf
            l1 `shouldBe` 1:l0
            _ <- stToIO do STBuffer.consumeHead buf
            l2 <- stToIO do STBuffer.toList buf
            l2 `shouldBe` l0

        it "be satisfied for consumeLast / appendLast" do
            let l0 = [0,0,0,0,0,0,0] :: [Int]
            buf <- stToIO do STBuffer.new l0
            stToIO do STBuffer.appendLast 1 buf
            l1 <- stToIO do STBuffer.toList buf
            l1 `shouldBe` l0 <> [1]
            _ <- stToIO do STBuffer.consumeLast buf
            l2 <- stToIO do STBuffer.toList buf
            l2 `shouldBe` l0




