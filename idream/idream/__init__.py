#!/usr/bin/env python

from __future__ import print_function
import json
import os
import subprocess
import sys


_ROOT = os.path.abspath(os.path.dirname(os.path.dirname(__file__)))
_IDRIS_COMMANDS = set(['build', 'install', 'repl', 'clean', 'mkdoc', 'checkpkg'])


def read_packages():
    packages_path = os.path.join(_ROOT, 'packages.json')
    with open(packages_path, 'r') as f:
        return json.load(f)


def read_package(project):
    package_path = os.path.join(_ROOT, 'internal', project, 'package.json')
    with open(package_path, 'r') as f:
        return json.load(f)


def generate(project, package):
    modules = package.get('modules', [])
    pkgs = package.get('pkgs', [])
    executable = package.get('executable', False)

    ipkg_path = os.path.join(_ROOT, 'internal', project, '{}.ipkg'.format(project))

    with open(ipkg_path, 'w') as f:
        f.write('package {}\n'.format(project))
        f.write('sourcedir = src\n')
        if modules:
            f.write('modules = {}\n'.format(', '.join(modules)))
        if executable:
            f.write('executable = {}\n'.format(project))
            f.write('main = Main\n')
        if pkgs:
            f.write('pkgs = {}\n'.format(', '.join(pkgs)))


def system(command, cwd=None):
    print('+', command)
    subprocess.call(command, shell=True, cwd=cwd)


def idris(location, project, command, package):
    pkgs = package.get('pkgs', [])
    executable = package.get('executable', False)

    project_path = os.path.join(_ROOT, location, project)
    output_path = os.path.join(_ROOT, 'output')
    bin_path = os.path.join(output_path, 'bin')
    lib_path = os.path.join(output_path, 'lib')
    project_lib_path = os.path.join(lib_path, project)
    os.makedirs(bin_path, exist_ok=True)
    os.makedirs(project_lib_path, exist_ok=True)

    orig_idrispath = subprocess.run(['idris', '--libdir'], stdout=subprocess.PIPE).stdout.decode('utf-8').strip()
    idrispaths = []
    for pkg in os.listdir(orig_idrispath):
        idrispaths.append(os.path.join(orig_idrispath, pkg))
    for pkg in pkgs:
        pkg_output_path = os.path.join(output_path, 'lib', pkg)
        idrispaths.append(pkg_output_path)
    idrispaths_arg = ' '.join('--idrispath {}'.format(p) for p in idrispaths)

    idris = 'idris --ibcsubdir {3} {2} --verbose --{1} {0}.ipkg'.format(
        project, command, idrispaths_arg, project_lib_path)
    system(idris, cwd=project_path)

    generated_bin = os.path.join(project_path, project)
    if executable and os.path.isfile(generated_bin):
        os.rename(generated_bin, os.path.join(bin_path, project))


def fetch(project, package):
    ext_path = os.path.join(_ROOT, 'external')
    os.makedirs(ext_path, exist_ok=True)
    project_path = os.path.join(ext_path, project)
    if not os.path.isdir(project_path):
        source = package['source']
        tag = package['tag']
        system('git clone {}'.format(source), cwd=ext_path)
        system('git checkout {}'.format(tag), cwd=project_path)
    else:
        print('skipping', project)


def dispatch(location, project, command, package):
    if command == 'fetch':
        assert location == 'external'
        fetch(project, package)
    elif command in _IDRIS_COMMANDS:
        if location == 'internal':
            generate(project, package)
        idris(location, project, command, package)
    else:
        raise Exception('Unknown command', command)


def main():
    assert len(sys.argv) == 4
    location = sys.argv[1]
    project = sys.argv[2]
    command = sys.argv[3]

    packages = read_packages()
    assert project in packages[location]
    if location == 'internal':
        package = read_package(project)
        assert package['name'] == project
    else:
        package = packages[location][project]

    dispatch(location, project, command, package)


if __name__ == '__main__':
    main()
