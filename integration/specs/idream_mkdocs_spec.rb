require_relative 'idream_common'

describe 'idream mkdocs ipkg command' do

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
    output = mkdocs
    output_after = `ls -a`

    expect(output_after).to eq(output_before)
    expect(output_after).not_to include('.idream-work')
    expect(output).to include('Failed to read from file (idr-project.json)')
  end

  it "gives an informational message when project has no packages yet" do
    output = mkdocs
    expect(output).to include('Project contains no packages yet')
    expect(output).to include('Use `idream add` to add a package to this project first.')
  end

  it "gives an informational message when it can't find the dependency graph file" do
    idream "add --lib #{lib_name}"
    expect(mkdocs).to include('Failed to read from file (.idream-work/dependency-graph.json)')
  end

  describe "generation of docs of projects (happy path)" do

    before do
      idream "add --exe #{exe_name}"
      File.write 'idr-package-set.json', pkg_set_contents
      File.write (File.join exe_name, 'idr-package.json'), pkg_contents
      # Needed because Main file is not documented by Idris doc?
      File.write (File.join exe_name, 'src', 'Extra.idr'), extra_contents
      idream 'fetch'
      idream 'generate-ipkg'
      idream 'compile'  # TODO make this step obsolete, otherwise complains about not finding code used from the base packages..
    end

    it "generates docs for each of the fetched packages" do
      output = mkdocs
      expect(output).to include('Generating documentation for package: package1')
      expect(output).to include('Generating documentation for package: package2')
      expect(output).to include('Generating documentation for package: package3')
      expect(output).to include('Generating documentation for package: test_exe')
    end
  end


  # Helper functions

  def mkdocs
    idream '--log-level debug mkdocs'
  end

  def extra_contents
    <<~END
    module Extra

    %access public export

    ||| bla
    x : a -> a
    x a = a
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
        "repo": "https://github.com/idream-build/idream_test_dep1.git",
        "version": "master"
      },
      "test_dependency2": {
        "repo": "https://github.com/idream-build/idream_test_dep2.git",
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
