require_relative 'idream_common'

describe 'idream add command' do

  before do
    @idream = idream_exe
    @idream_dir = Dir.pwd
    Dir.mkdir test_dir
    Dir.chdir test_dir
    idream "new #{proj_name}"
    Dir.chdir File.join(test_dir, proj_name)
  end

  after do
    Dir.chdir @idream_dir
    `rm -rf #{test_dir}`
  end

  it "fails when no args are provided" do
    output = idream "add"
    expect(output).to include('Missing: PACKAGE_NAME (--lib | --exe)')
    expect(output).to include('Usage: idream add PACKAGE_NAME (--lib | --exe)')
  end

  it "fails when --lib or --exe missing" do
    output = idream "add #{lib_name}"
    expect(output).to include('Missing: (--lib | --exe)')
    expect(output).to include('Usage: idream add PACKAGE_NAME (--lib | --exe)')
  end

  it "fails when invalid flag is passed in" do
    expect(idream "add --invalid").to include("Invalid option `--invalid'")
  end

  it "fails when both --lib and --exe flags are passed in" do
    expect(idream "add --lib test --exe test2").to include("Invalid option")
  end

  it "shows an informational message when a new library is successfully initialized" do
    output = idream "add --lib #{lib_name}"
    expect(output).to include("Successfully added package #{lib_name} to project.")
  end

  it "shows an informational message when a new executable is successfully initialized" do
    output = idream "add --exe #{lib_name}"
    expect(output).to include("Successfully added package #{lib_name} to project.")
  end

  it "generates the expected files for a library" do
    idream "add --lib #{lib_name}"

    expect(File.directory? "./#{lib_name}").to be(true)
    expect(File.directory? "./#{lib_name}/src").to be(true)
    ["./idr-project.json",
     "./#{lib_name}/idr-package.json",
     "./#{lib_name}/src/Lib.idr"
    ].zip([
      idr_project_contents(proj_name, lib_name),
      idr_package_contents(lib_name, false),
      lib_idr_contents
    ]).each do |f, contents|
      expect(File.read f).to include(contents)
    end
  end

  it "generates the expected files for an executable" do
    idream "add --exe #{exe_name}"

    expect(File.directory? "./#{exe_name}").to be(true)
    expect(File.directory? "./#{exe_name}/src").to be(true)
    ["./idr-project.json",
     "./#{exe_name}/idr-package.json",
     "./#{exe_name}/src/Main.idr"
    ].zip([
      idr_project_contents(proj_name, exe_name),
      idr_package_contents(exe_name, true),
      main_idr_contents
    ]).each do |f, contents|
      expect(File.read f).to include(contents)
    end
  end

  it "fails when trying to add package with conflicting names" do
    idream "add --exe #{exe_name}"
    output = idream "add --exe #{exe_name}"
    error = "Failed to add package to project, package #{exe_name} already exists"
    expect(output).to include(error)
  end

  it "can add multiple packages" do
    idream "add --exe #{exe_name}"
    idream "add --lib #{lib_name}"
    proj_info = File.read './idr-project.json'
    expect(proj_info).to include(exe_name)
    expect(proj_info).to include(lib_name)
  end


  # Helper functions

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

  def idr_project_contents proj, lib
    <<~END
    {
        "packages": [
            "#{lib}"
        ],
        "project_name": "#{proj}"
    }
    END
  end

  def idr_package_contents lib, is_library
    <<~END
    {
        "name": "#{lib}",
        "source_dir": "src",
        "executable": #{is_library},
        "dependencies": []
    }
    END
  end

  def main_idr_contents
    <<~END
    module Main

    ||| Main program, to be replaced with actual code.
    main : IO ()
    main = putStrLn \"Hello, Idris!\"
    END
  end

  def lib_idr_contents
    <<~END
    module Lib

    ||| Library function, to be replaced with actual code.
    libFunction : String
    libFunction = "Hello, Idris!"
    END
  end
end
