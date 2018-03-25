describe 'idream new command' do
  # TODO split up into init/new?

  before(:each) do
    @idream = idream_exe
    @idream_dir = Dir.pwd
    Dir.mkdir test_dir
    Dir.chdir test_dir
  end

  after do
    Dir.chdir @idream_dir
    `rm -rf #{test_dir}`
  end

  it "throws an error when --lib or --exe missing" do
    expect(idream "new").to eq args_missing
    expect(idream "new test_project").to eq args_missing
    expect(idream "new --invalid").to include("Invalid option `--invalid'")
  end

  it "can generate a project template for a library" do
    lib_name = 'test_lib'
    expect(idream "new --lib #{lib_name}").to eq ""  # TODO should output text!

    expect(File.directory? "./#{lib_name}").to be(true)
    expect(File.directory? "./#{lib_name}/#{lib_name}").to be(true)
    expect(File.directory? "./#{lib_name}/#{lib_name}/src").to be(true)
    ["./#{lib_name}/idr-package-set.json",
     "./#{lib_name}/idr-project.json",
     "./#{lib_name}/#{lib_name}/idr-package.json",
     "./#{lib_name}/#{lib_name}/src/Lib.idr"
    ].zip([
      idr_package_set_contents,
      idr_project_contents(lib_name),
      idr_package_contents(lib_name),
      lib_idr_contents
    ]).each do |f, contents|
      expect(File.read f).to include(contents)
    end
  end

  it "can generate a project template for an executable" do
    expect(false).to be_truthy
  end

  # Helper functions

  def idream_exe
    rel_path = `find . | grep dist | grep idream$ | grep -v idream-tmp`.strip
    File.absolute_path rel_path
  end

  def idream args
    `#{@idream} #{args} 2>&1`
  end

  def test_dir
    '/tmp/idream_integration_tests'
  end

  def args_missing
    <<~END
    Missing: (--lib | --exe)

    Usage: idream new (--lib | --exe)
      Initializes a new project.
    END
  end

  def idr_package_set_contents
    "{}\n"  # empty by default
  end

  def idr_project_contents lib_name
    <<~END
    {
        "packages": [
            "#{lib_name}"
        ]
    }
    END
  end

  def idr_package_contents lib_name
    <<~END
    {
        "name": "#{lib_name}",
        "sourcedir": "src",
        "executable": false,
        "dependencies": []
    }
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
