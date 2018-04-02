require_relative 'idream_common'

describe 'idream fetch command' do

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

  it "does nothing when used outside of an idream project" do
    Dir.chdir '/tmp'

    output_before = `ls -a`
    output = fetch
    output_after = `ls -a`

    expect(output_before).not_to include('.idream-work')
    expect(output_after).not_to include('idream-work')
    expect(output).to include('Did not find project file, aborting.')
  end

  it "does nothing if a project has no subpackages (nothing needed to build)" do
    File.write 'idr-package-set.json', pkg_set_contents

    output = fetch
    output_after = `ls -a .idream-work`

    expect(output).to include("Project contains no packages yet, skipping fetch step")
    expect(output_after).not_to include('idris-array')
  end

  it "doesn't fail when no deps required to fetch" do
    idream "add --lib #{lib_name}"

    output_before = `ls -a .idream-work`
    output = fetch
    output_after = `ls -a .idream-work`

    expect(output_before).not_to eq(output_after)
    expect(output).to include("Fetching dependencies for #{proj_name}")
    expect(output).to include("Fetching dependencies for package: #{lib_name}")
    expect(output).to include("Successfully fetched dependencies!")
    expect(output_after).to include('dependency-graph.json')
  end

  describe "fetch command with subpackage in project" do

    before do
      idream "add --lib #{lib_name}"
    end

    it "will report an error if a dependency is missing in the package set" do
      File.write (File.join lib_name, 'idr-package.json'), pkg_contents_simple
      expect(fetch).to include("Package missing")
    end

    it "can successfully fetch non-recursive dependencies" do
      File.write 'idr-package-set.json', pkg_set_contents
      File.write (File.join lib_name, 'idr-package.json'), pkg_contents_simple

      output = fetch
      output_after = `ls -a .idream-work/src`

      expect(output).to include("Successfully fetched dependencies!")
      expect(output).to include("Fetching dependencies for package: test_lib")
      expect(output).to include("Fetching dependencies for package: package1")
      expect(output).to include("Fetching dependencies for package: package2")
      expect(output_after).to include('test_dependency1')
    end

    it "can successfully fetch recursive dependencies" do
      File.write 'idr-package-set.json', pkg_set_contents
      File.write (File.join lib_name, 'idr-package.json'), pkg_contents_complex

      output = fetch
      output_after = `ls -a .idream-work/src`

      expect(output).to include("Successfully fetched dependencies!")
      expect(output).to include("Fetching dependencies for package: test_lib")
      expect(output).to include("Fetching dependencies for package: package1")
      expect(output).to include("Fetching dependencies for package: package2")
      expect(output).to include("Fetching dependencies for package: package3")
      expect(output_after).to include('test_dependency1')
      expect(output_after).to include('test_dependency2')
    end
  end

    # Helper functions

  def fetch
    idream '--log-level debug fetch'
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

  def pkg_contents_simple
    <<~END
    {
        "name": "test_lib",
        "source_dir": "src",
        "executable": false,
        "dependencies": ["test_dependency1"]
    }
    END
  end

  def pkg_contents_complex
    <<~END
    {
        "name": "test_lib",
        "source_dir": "src",
        "executable": false,
        "dependencies": [
            "test_dependency1",
            "test_dependency2"
        ]
    }
    END
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
end
