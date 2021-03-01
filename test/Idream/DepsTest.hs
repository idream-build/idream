module Idream.DepsTest where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Idream.Deps (Deps (..), closureDeps, composeAndUnionDeps, composeDeps, depsFromEdges, depsFromGroups,
                    depsFromPath, depsVertices, linearizeDeps, restrictDeps, revDeps)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

type D = Deps Int Int

test_deps :: TestTree
test_deps = testCase "deps" $ do
  let dlist = [(1, Set.fromList [2]), (2, Set.fromList [3])]
      dverts = Set.fromList [1, 2, 3]
      d1 :: D = depsFromPath [1, 2, 3]
      d2 :: D = depsFromEdges [(1, 2), (2, 3)]
      d3 :: D = depsFromGroups dlist
  d1 @?= d2
  d1 @?= d3
  depsEdges d1 @?= Map.fromList dlist
  depsKeys d1 @?= Set.fromList [1, 2]
  depsValues d1 @?= Set.fromList [2, 3]
  depsVertices d1 @?= dverts
  let xlist = [(2, Set.fromList [1]), (3, Set.fromList [2])]
      x1 :: D = revDeps d1
      x2 :: D = depsFromPath [3, 2, 1]
  x1 @?= x2
  depsEdges x1 @?= Map.fromList xlist
  depsKeys x1 @?= Set.fromList [2, 3]
  depsValues x1 @?= Set.fromList [1, 2]
  depsVertices x1 @?= dverts
  revDeps x1 @?= d1
  let c1 :: D = composeDeps d1 d1
      c2 :: D = depsFromEdges [(1, 3)]
  depsEdges c1 @?= depsEdges c2
  let e1 :: D = closureDeps d1
      e2 :: D = depsFromEdges [(1, 2), (1, 3), (2, 3)]
  e1 @?= e2
  c1 <> d1 @?= e1
  d1 <> c1 @?= e1
  let f1 :: D = composeAndUnionDeps d1
  f1 @?= e1
  linearizeDeps d1 @?= [3, 2, 1]
  linearizeDeps x1 @?= [1, 2, 3]
  let s :: D = depsFromPath [3]
  depsEdges s @?= Map.empty
  depsKeys s @?= Set.empty
  depsValues s @?= Set.singleton 3
  depsVertices s @?= Set.singleton 3
  restrictDeps (const True) d1 @?= d1
  restrictDeps (const False) d1 @?= mempty
  restrictDeps (\k -> k == 1) d1 @?= d1
  restrictDeps (\k -> k == 2) d1 @?= depsFromPath [2, 3]
  restrictDeps (\k -> k == 3) d1 @?= depsFromPath [3]
