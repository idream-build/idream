
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

module Idream.GraphSpec where

import Test.Tasty.Hspec
import Algebra.Graph as Graph
import qualified Data.Map as Map
import Idream.Graph ( getLeafNodes
                    , createBuildPlan
                    , Max(..)
                    , Depth
                    , MonoidMap(..)
                    , BuildPlan )


g1, g2, g1', g2' :: Graph.Graph Char

g1 = path ['A', 'B', 'C']

g2 = overlays [acdef, abef, abg] where
  acdef = path ['A', 'C', 'D', 'E', 'F']
  abef = path ['A', 'B', 'E', 'F']
  abg = path ['A', 'B', 'G']

g1' = transpose g1

g2' = transpose g2

mkMonoidMap :: Ord a => [(a, Max Depth)] -> BuildPlan a
mkMonoidMap = MonoidMap . Map.fromList


spec_getLeafs :: Spec
spec_getLeafs =
  describe "finding the leaf nodes in a graph (no outward pointing arrows)" $
    it "should find the correct list of nodes in the graph" $ do
      getLeafNodes g1 `shouldBe` ['C']
      getLeafNodes g1' `shouldBe` ['A']
      getLeafNodes g2 `shouldBe` ['F', 'G']
      getLeafNodes g2' `shouldBe` ['A']


spec_buildPlanFromGraph :: Spec
spec_buildPlanFromGraph =
  describe "constructing build plan from a graph" $
    it "creates a sorted list from the graph" $ do
      createBuildPlan g1 `shouldBe` mkMonoidMap [ ('A', Max 2)
                                                , ('B', Max 1)
                                                , ('C', Max 0)
                                                ]
      createBuildPlan g1' `shouldBe` mkMonoidMap [ ('A', Max 0)
                                                 , ('B', Max 1)
                                                 , ('C', Max 2)
                                                 ]
      createBuildPlan g2 `shouldBe` mkMonoidMap [ ('A', Max 4)
                                                , ('B', Max 2)
                                                , ('C', Max 3)
                                                , ('D', Max 2)
                                                , ('E', Max 1)
                                                , ('F', Max 0)
                                                , ('G', Max 0)
                                                ]
      createBuildPlan g2' `shouldBe` mkMonoidMap [ ('A', Max 0)
                                                 , ('B', Max 1)
                                                 , ('C', Max 1)
                                                 , ('D', Max 2)
                                                 , ('E', Max 3)
                                                 , ('F', Max 4)
                                                 , ('G', Max 2)
                                                 ]
