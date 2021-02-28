module Idream.DepsTest where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Idream.Deps (Deps (..), closureDeps, composeDeps, depsFromEdges, depsFromGroups, depsFromPath, revDeps)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

type D = Deps Int Int

test_deps :: TestTree
test_deps = testCase "deps" $ do
  let dlist = [(1, Set.fromList [2]), (2, Set.fromList [3])]
      d1 :: D = depsFromPath [1, 2, 3]
      d2 :: D = depsFromEdges [(1, 2), (2, 3)]
      d3 :: D = depsFromGroups dlist
  d1 @?= d2
  d1 @?= d3
  unDeps d1 @?= Map.fromList dlist
  let x1 :: D = revDeps d1
      x2 :: D = depsFromPath [3, 2, 1]
  x1 @?= x2
  revDeps x1 @?= d1
  let c1 :: D = composeDeps d1 d1
      c2 :: D = depsFromEdges [(1, 3)]
  c1 @?= c2
  let e1 :: D = closureDeps d1
      e2 :: D = depsFromEdges [(1, 2), (1, 3), (2, 3)]
  e1 @?= e2
  c1 <> d1 @?= e1
  d1 <> c1 @?= e1
