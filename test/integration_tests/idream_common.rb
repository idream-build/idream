
# This file contains some common helper functions used in the tests.

def idream_exe
  rel_path = `find . | grep dist | grep idream$ | grep -v idream-tmp`.strip
  File.absolute_path rel_path
end

def test_dir
  '/tmp/idream_integration_tests'
end
