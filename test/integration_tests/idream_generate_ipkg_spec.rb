require_relative 'idream_common'

describe 'idream generate ipkg command' do

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
    output = generate
    output_after = `ls -a`

    expect(output_after).to eq(output_before)
    expect(output_after).not_to include('.idream-work')
    expect(output).to include('Failed to read from file (idr-project.json)')
  end

  it "gives an informational message when no packages yet" do
    output = generate
    expect(output).to include('Project contains no packages yet')
    expect(output).to include('Use `idream add` to add a package to this project first.')
  end

  it "gives an informational message when it can't find the dependency graph file" do
    graph_file = '.idream-work/dependency-graph.json'
    idream "add --lib #{lib_name}"
    expect(generate).to include("Failed to read from file (#{graph_file})")
  end

  describe "generation of ipkg files" do

    before do
      idream "add --exe #{exe_name}"
      File.write 'idr-package-set.json', pkg_set_contents
      File.write (File.join exe_name, 'idr-package.json'), pkg_contents
      idream 'fetch'
    end

    it "generates a .ipkg file for each of the fetched packages" do
      output = generate
      exe_ipkg = File.read (File.join exe_build_dir, "#{exe_name}.ipkg")
      pkg1_ipkg = File.read (File.join pkg1_build_dir, "package1.ipkg")

      expect(output).to include('package1.ipkg')
      expect(output).to include('package2.ipkg')
      expect(output).to include('package3.ipkg')
      expect(output).to include("#{exe_name}.ipkg")

      expect(exe_ipkg).to include(exe_ipkg_contents)
      expect(pkg1_ipkg).to include(pkg1_ipkg_contents)
    end
  end


  # Helper functions

  def generate
    idream '--log-level debug generate-ipkg'
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
    package test_dependency1_package1
    -- NOTE: This is an auto-generated file by idream. Do not edit.
    modules = Package1.Lib, Package1.Extras

    sourcedir = src
    END
  end

  def exe_ipkg_contents
    <<~END
    package test_project_test_exe
    -- NOTE: This is an auto-generated file by idream. Do not edit.
    modules = Main
    pkgs = test_dependency1_package1, test_dependency1_package2, test_dependency2_package3
    sourcedir = src
    executable = test_exe
    main = Main
    END
  end
end
