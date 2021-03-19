module Idream.MiscTest where

import Idream.Types.Ipkg (ModuleName (..), extractModuleName)
import Idream.Prelude
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

test_modules :: TestTree
test_modules = testCase "modules" $ do
  let fileName = "Package1/Foo.idr"
      expectedModName = ModuleName ["Package1", "Foo"]
      expectedModRendered = "Package1.Foo"
      actualModName = extractModuleName fileName
      actualModRendered = toText actualModName
  expectedModName @?= actualModName
  expectedModRendered @?= actualModRendered
