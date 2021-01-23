module Language.Quell.Parsing.Lexer.RulesSpec where

import           Language.Quell.Prelude
import           Test.Hspec

import           Language.Quell.Parsing.Lexer.Rules

import qualified Language.Lexer.Tlex.Data.EnumSet      as EnumSet
import qualified Language.Quell.Parsing.Lexer.CodeUnit as CodeUnit


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

        it "(graphic | whitechar) are including most common categories" do
            let diffCs = mconcat
                    [
                        CodeUnit.catUppercaseLetter,
                        CodeUnit.catModifierLetter,
                        CodeUnit.catLowercaseLetter,
                        CodeUnit.catTitlecaseLetter,
                        CodeUnit.catOtherLetter,
                        CodeUnit.catMark,
                        CodeUnit.catDecimalNumber,
                        CodeUnit.catLetterNumber,
                        CodeUnit.catOtherNumber,
                        CodeUnit.catPunctuation,
                        CodeUnit.catSymbol,
                        CodeUnit.catParagraphSeparator,
                        CodeUnit.catLineSeparator,
                        CodeUnit.catSpaceSeparator,
                        CodeUnit.catFormat
                    ] `EnumSet.difference` mconcat [
                        graphicCs,
                        whiteCharCs
                    ]
            diffCs `shouldBe` mempty

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
