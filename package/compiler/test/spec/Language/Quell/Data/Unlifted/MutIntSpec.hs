module Language.Quell.Data.Unlifted.MutIntSpec (spec) where

import           Language.Quell.Prelude
import           Test.Hspec

import           Language.Quell.Data.Unlifted.MutInt

import qualified GHC.ST                              as ST


spec :: Spec
spec = do
    describe "new#" do
        it "returns allocated integer with specified integer" do
            let i = runRawST \s0# ->
                    let !(# s1#, mi# #) = new# 17# s0#
                        !(# s2#, i# #) = read# mi# s1#
                    in (# s2#, I# i# #)
            i `shouldBe` 17

    describe "write#" do
        it "overwrites allocted integer with specified integer" do
            let i = runRawST \s0# ->
                    let !(# s1#, mi# #) = new# 17# s0#
                        s2# = write# mi# 19# s1#
                        !(# s3#, i# #) = read# mi# s2#
                    in (# s3#, I# i# #)
            i `shouldBe` 19

runRawST :: (forall s. State# s -> (# State# s, a #)) -> a
runRawST f = runST do ST.ST f
