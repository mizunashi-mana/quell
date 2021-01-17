module Language.Quell.Parsing.Lexer.RulesSpec (spec) where

import           Language.Quell.Prelude
import           Test.Hspec

import           Language.Quell.Parsing.Lexer.Rules

import qualified Language.Lexer.Tlex.Data.EnumSet    as EnumSet


spec :: Spec
spec = do
    describe "Lexical nonterminals" do
        it "'s charset units should be disjoint" do
            assertCharSetsAreDisjoint
                [
                    whiteCharCs,
                    smallCs,
                    largeCs,
                    symbolCs,
                    digitCs,
                    otherCs,
                    specialCs,
                    otherSpecialCs,
                    otherGraphicCs
                ]

            assertCharSetsAreDisjoint
                [
                    spaceCs,
                    newlineCs
                ]

assertCharSetsAreDisjoint :: [CharSet] -> Expectation
assertCharSetsAreDisjoint css0 = go mempty css0 where
    go ucss = \case
        []     -> pure ()
        cs:css -> do
            let ocs = mconcat css <> ucss
                ics = cs `EnumSet.intersection` ocs
            ics `shouldBe` mempty
            go
                do cs <> ucss
                css
