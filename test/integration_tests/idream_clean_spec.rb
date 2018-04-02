require_relative 'idream_common'

describe 'idream clean command' do

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
    expected_output = `ls -a`
    idream 'clean'
    actual_output = `ls -a`
    expect(expected_output).not_to include('.idream-work')
    expect(actual_output).to eq(expected_output)
  end

  it "removes build dir when in an idream project (no packages)" do
    output_before = `ls -a`
    idream 'clean'
    output_after = `ls -a`

    expect(output_before).to include('.idream-work')
    expect(output_after).not_to include('.idream-work')
  end

  it "removes build dir when in an idream project (with packages)" do
    idream "add --lib #{lib_name}"
    idream "add --exe #{exe_name}"

    output_before = `ls -a`
    idream 'clean'
    output_after = `ls -a`

    expect(output_before).to include('.idream-work')
    expect(output_after).not_to include('.idream-work')
  end
end
