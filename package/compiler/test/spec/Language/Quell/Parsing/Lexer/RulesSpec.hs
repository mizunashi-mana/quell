module Language.Quell.Parsing.Lexer.RulesSpec (spec) where

import Language.Quell.Prelude
import Test.Hspec

import Language.Quell.Parsing.Lexer.Rules

import qualified Data.CharSet as CharSet


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

assertCharSetsAreDisjoint :: [CharSet.CharSet] -> Expectation
assertCharSetsAreDisjoint css0 = go mempty css0 where
  go ucss = \case
    []     -> pure ()
    cs:css -> do
      let ocs = mconcat css <> ucss
          ics = cs `CharSet.intersection` ocs
      ics `shouldBe` mempty
      go
        do cs <> ucss
        css
