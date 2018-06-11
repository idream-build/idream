
# This file contains some common helper functions used in the tests.

def idream_exe
  rel_path = `find . | grep dist | grep idream$ | grep -v idream-tmp`.strip
  File.absolute_path rel_path
end

def test_dir
  '/tmp/idream_integration_tests'
end

def idream args
  `#{@idream} #{args} 2>&1`
end

def proj_name
  'test_project'
end

def lib_name
  'test_lib'
end

def exe_name
  'test_exe'
end
