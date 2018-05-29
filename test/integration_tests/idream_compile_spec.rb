require_relative 'idream_common'

describe 'idream compile ipkg command' do

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

  it "does nothing when not in an idream project" do
    Dir.chdir '/tmp'
    output_before = `ls -a`
    output = compile
    output_after = `ls -a`

    expect(output_after).to eq(output_before)
    expect(output_after).not_to include('.idream-work')
    expect(output).to include('Failed to read from file (idr-project.json)')
  end

  it "gives an informational message when project has no packages yet" do
    output = compile
    expect(output).to include('Project contains no packages yet')
    expect(output).to include('Use `idream add` to add a package to this project first.')
  end

  it "gives an informational message when it can't find the dependency graph file" do
    idream "add --lib #{lib_name}"
    expect(compile).to include('Failed to read from file (.idream-work/dependency-graph.json)')
  end

  describe "compilation of projects (happy path)" do

    before do
      idream "add --exe #{exe_name}"
      File.write 'idr-package-set.json', pkg_set_contents
      File.write (File.join exe_name, 'idr-package.json'), pkg_contents
      idream 'fetch'
      idream 'generate-ipkg'
    end

    it "compiles an .ipkg file for each of the fetched packages" do
      output = compile
      expect(output).to include('Compiled package: package1')
      expect(output).to include('Compiled package: package2')
      expect(output).to include('Compiled package: package3')
      expect(output).to include('Compiled package: test_exe')
      expect(output).to include('Successfully compiled package(s)!')
    end
  end

  describe "compilation of projects (error situations)" do

    before do
      idream "add --exe #{exe_name}"
      File.write 'idr-package-set.json', pkg_set_contents
      File.write (File.join exe_name, 'idr-package.json'), bad_pkg_contents
      File.write (File.join exe_name, 'src', 'Main.idr'), bad_main_contents
      idream 'fetch'
      idream 'generate-ipkg'
    end

    it "shows error output when it can't compile a package" do
      output = compile
      expect(output).to include('Compiled package: package1')
      expect(output).to include('Compiled package: package2')
      expect(output).to include('Failed to invoke idris')
      expect(output).to include("Can't find import Package3/Lib")
    end
  end


  # Helper functions

  def compile
    idream '--log-level debug compile'
  end

  def bad_main_contents
    <<~END
    module Main

    import Package1.Lib
    import Package2.Lib
    import Package3.Lib

    ||| Main program, to be replaced with actual code.
    main : IO ()
    main = putStrLn "Hello, Idris!"
    END
  end

  def bad_pkg_contents
    # NOTE: misses a dependency!
    <<~END
    {
        "name": "test_exe",
        "source_dir": "src",
        "executable": true,
        "dependencies": [
            {"project": "test_dependency1", "package": "package1"},
            {"project": "test_dependency1", "package": "package2"}
        ]
    }
    END
  end

  def pkg_contents
    <<~END
    {
        "name": "test_exe",
        "source_dir": "src",
        "executable": true,
        "dependencies": [
            {"project": "test_dependency1", "package": "package1"},
            {"project": "test_dependency1", "package": "package2"},
            {"project": "test_dependency2", "package": "package3"}
        ]
    }
    END
  end

  def pkg_set_contents
    <<~END
    {
      "test_dependency1": {
        "repo": "https://github.com/luc-tielen/idream_test_dep1.git",
        "version": "master"
      },
      "test_dependency2": {
        "repo": "https://github.com/luc-tielen/idream_test_dep2.git",
        "version": "master"
      }
    }
    END
  end

  def pkg1_build_dir
    File.join '.idream-work', 'build', 'test_dependency1', 'package1'
  end

  def exe_build_dir
    File.join '.idream-work', 'build', proj_name, exe_name
  end

  def pkg1_ipkg_contents
    <<~END
    -- NOTE: This is an auto-generated file by idream. Do not edit.
    package package1
    modules = Lib, Extras

    sourcedir = src
    END
  end

  def exe_ipkg_contents
    <<~END
    -- NOTE: This is an auto-generated file by idream. Do not edit.
    package test_exe
    modules = Main
    pkgs = test_dependency1_package1, test_dependency1_package2, test_dependency2_package3
    sourcedir = src
    executable = test_exe
    main = Main
    END
  end
end
