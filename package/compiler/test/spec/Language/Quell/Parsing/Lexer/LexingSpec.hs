module Language.Quell.Parsing.Lexer.LexingSpec where

import           Language.Quell.Prelude
import           Test.Hspec

import           Language.Quell.Parsing.Lexer.Lexing

import qualified Language.Lexer.Tlex.Data.EnumSet      as EnumSet
import qualified Language.Quell.Parsing.Lexer.CodeUnit as CodeUnit
import qualified Language.Quell.Parsing.Lexer.Rules    as Rules


spec :: Spec
spec = do
    describe "lexDigitChar" do
        it "are lexing digitCs only" do
            let us = EnumSet.fromList
                    [ CodeUnit.fromChar c
                    | c <- [ minBound .. maxBound ]
                    , lexDigitChar c /= Nothing
                    ]
            us `shouldBe` Rules.digitCs

        it "'s result >= 0" do
            forM_ [minBound .. maxBound] \c ->
                lexDigitChar c `shouldSatisfy` \case
                    Nothing -> True
                    Just n  -> n >= 0
