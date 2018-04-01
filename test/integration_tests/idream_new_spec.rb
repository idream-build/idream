require_relative 'idream_common'

describe 'idream new command' do
  before do
    @idream = idream_exe
    @idream_dir = Dir.pwd
    Dir.mkdir test_dir
    Dir.chdir test_dir
  end

  after do
    Dir.chdir @idream_dir
    `rm -rf #{test_dir}`
  end

  it "fails when no args are provided" do
    output = idream "new"
    expect(output).to include('Missing: PROJECT_NAME')
    expect(output).to include('Usage: idream new PROJECT_NAME')
  end

  it "fails when multiple args provided" do
    expect(idream "new test test2").to include("Invalid argument `test2'")
  end

  it "fails when invalid flag is passed in" do
    expect(idream "new --invalid").to include("Invalid option `--invalid'")
  end

  it "gives an informational message on success." do
    output = idream "new #{proj_name}"
    expect(output).to include("Successfully initialized project: #{proj_name}.")
  end

  it "generates the expected directory structure for projects" do
    idream "new #{proj_name}"

    expect(File.directory? "./#{proj_name}").to be(true)
    expect(File.directory? "./#{proj_name}/.idream-work").to be(true)
    ["./#{proj_name}/.gitignore",
     "./#{proj_name}/idr-package-set.json",
     "./#{proj_name}/idr-project.json",
    ].zip([
      gitignore_contents,
      idr_package_set_contents,
      idr_project_contents(proj_name)
    ]).each do |f, contents|
      expect(File.read f).to include(contents)
    end
  end


  # Helper functions

  def idream args
    `#{@idream} #{args} 2>&1`
  end

  def proj_name
    'test_project'
  end

  def idr_package_set_contents
    "{}\n"  # empty by default
  end

  def idr_project_contents project_name
    <<~END
    {
        "project_name": "#{project_name}",
        "packages": [

        ]
    }
    END
  end

  def gitignore_contents
    ".idream-work/\n"
  end
end
