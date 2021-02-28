
-- {-# LANGUAGE Rank2Types #-}

module Idream.GraphSpec where

-- import Algebra.Graph as Graph
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import Idream.Graph (BuildPlan (..), Depth, Max (..), MonoidMap (..), createBuildPlan, getLeafNodes, mkBuildPlan)
-- import Test.Tasty.Hspec


-- g1, g2, g1', g2' :: Graph.Graph Char

-- g1 = path ['A', 'B', 'C']

-- g2 = overlays [acdef, abef, abg] where
--   acdef = path ['A', 'C', 'D', 'E', 'F']
--   abef = path ['A', 'B', 'E', 'F']
--   abg = path ['A', 'B', 'G']

-- g1' = transpose g1

-- g2' = transpose g2


-- mkMonoidMap :: Ord a => [(a, Max Depth)] -> MonoidMap a (Max Depth)
-- mkMonoidMap = MonoidMap . Map.fromList

-- mockBuildPlan :: Ord a => [[a]] -> BuildPlan a
-- mockBuildPlan as =
--   let len = length as
--       sets = map Set.fromList as
--       phases = Map.fromList $ zip [0, 1..] sets
--   in BuildPlan { numPhases = len, phaseMap = phases }


-- spec_getLeafs :: Spec
-- spec_getLeafs =
--   describe "finding the leaf nodes in a graph (no outward pointing arrows)" $
--     it "should find the correct list of nodes in the graph" $ do
--       getLeafNodes g1 `shouldBe` ['C']
--       getLeafNodes g1' `shouldBe` ['A']
--       getLeafNodes g2 `shouldBe` ['F', 'G']
--       getLeafNodes g2' `shouldBe` ['A']

-- spec_mkBuildPlan :: Spec
-- spec_mkBuildPlan =
--   describe "constructing build plan from a depth map" $
--     it "should group all items in same build phase together" $ do
--       let depthMap1 = mkMonoidMap ([] :: [(Char, Max Depth)])
--           depthMap2 = mkMonoidMap [('A', Max 0)]
--           depthMap3 = mkMonoidMap [('A', Max 0), ('B', Max 0)]
--           depthMap4 = mkMonoidMap [('A', Max 0), ('B', Max 0), ('C', Max 1)]
--       mkBuildPlan depthMap1 `shouldBe` mockBuildPlan ([] :: [[Char]])
--       mkBuildPlan depthMap2 `shouldBe` mockBuildPlan [['A']]
--       mkBuildPlan depthMap3 `shouldBe` mockBuildPlan [['A', 'B']]
--       mkBuildPlan depthMap4 `shouldBe` mockBuildPlan [['A', 'B'], ['C']]

-- spec_buildPlanFromGraph :: Spec
-- spec_buildPlanFromGraph =
--   describe "constructing build plan from a graph" $
--     it "creates a sorted list from the graph" $ do
--       createBuildPlan g1 `shouldBe` mockBuildPlan [['C'], ['B'], ['A']]
--       createBuildPlan g1' `shouldBe` mockBuildPlan [['A'], ['B'], ['C']]
--       createBuildPlan g2 `shouldBe` mockBuildPlan [ ['F', 'G']
--                                                   , ['E']
--                                                   , ['B', 'D']
--                                                   , ['C']
--                                                   , ['A']
--                                                   ]
--       createBuildPlan g2' `shouldBe` mockBuildPlan [ ['A']
--                                                    , ['B', 'C']
--                                                    , ['D', 'G']
--                                                    , ['E']
--                                                    , ['F']
--                                                    ]
