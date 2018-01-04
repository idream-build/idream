#!/usr/bin/env python3

import json
import os
import subprocess
import sys


_ROOT = os.path.dirname(os.path.dirname(__file__))
_IDRIS_COMMANDS = set(['build', 'test', 'clean', 'execute'])


def read_packages():
    packages_path = os.path.join(_ROOT, 'packages.json')
    with open(packages_path, 'r') as f:
        return json.load(f)


def read_package(project):
    package_path = os.path.join(_ROOT, 'internal', project, 'package.json')
    with open(package_path, 'r') as f:
        return json.load(f)


def generate(project):
    ipkg_path = os.path.join(_ROOT, 'internal', project, '{}.ipkg'.format(project))
    package = read_package(project)
    assert package['name'] == project
    pkgs = package.get('pkgs')
    executable = package.get('executable', False)
    with open(ipkg_path, 'w') as f:
        f.write('package {}\n'.format(project))
        f.write('sourcedir = src\n')
        if executable:
            f.write('executable = {}\n'.format(project))
            f.write('main = Main\n')
        if pkgs:
            f.write('pkgs = {}\n'.format(', '.join(pkgs)))


def system(command):
    print('+', command)
    os.system(command)


def sub_idris(location, project, pkgs, executable, command):
    project_path = os.path.join(_ROOT, location, project)
    output_path = os.path.join(_ROOT, 'output')
    bin_path = os.path.join(_ROOT, 'output', 'bin')
    project_output_path = os.path.join(output_path, 'lib', project)
    os.makedirs(bin_path, exist_ok=True)
    os.makedirs(project_output_path, exist_ok=True)
    orig_idrispath = subprocess.run(['idris', '--libdir'], stdout=subprocess.PIPE).stdout.decode('utf-8').strip()
    idrispaths = []
    for pkg in ['base', 'prelude']:
        idrispaths.append(os.path.join(orig_idrispath, pkg))
    for pkg in pkgs:
        idrispaths.append('../../../output/lib/{}'.format(pkg))
    idrispaths_arg = ' '.join('--idrispath {}'.format(p) for p in idrispaths)
    idris = 'cd {0} && idris --ibcsubdir ../../../output/lib/{1} {3} --{2} {1}.ipkg'.format(
        project_path, project, command, idrispaths_arg)
    system(idris)
    generated_bin = '{0}/{1}/{1}'.format(location, project)
    if executable and os.path.isfile(generated_bin):
        os.rename(generated_bin, os.path.join(bin_path, project))


def idris(location, project, command):
    if location == 'internal':
        package = read_package(project)
        sub_idris(
            location='internal',
            project=project,
            pkgs=package.get('pkgs', []),
            executable=package.get('executable', False),
            command=command)
    elif location == 'external':
        raise Exception('TODO')
    else:
        raise Exception('Unknown location', location)


def fetch(project, ext_package):
    ext_path = os.path.join(_ROOT, 'external')
    os.makedirs(ext_path, exist_ok=True)
    project_path = os.path.join(ext_path, project)
    if not os.path.isdir(project_path):
        cmd = 'cd {} && git clone {} && cd {} && git checkout {}'.format(
            ext_path, ext_package['source'], project, ext_package['tag'])
        system(cmd)
    else:
        print('skipping', project)


def dispatch(location, project, command):
    packages = read_packages()
    assert project in packages[location]
    if command == 'generate':
        assert location == 'internal'
        generate(project)
    elif command == 'fetch':
        assert location == 'external'
        ext_package = packages[location][project]
        fetch(project, ext_package)
    elif command in _IDRIS_COMMANDS:
        idris(location, project, command)
    else:
        raise Exception('Unknown command', command)


def main():
    packages_path = os.path.join(_ROOT, 'packages.json')
    with open(packages_path, 'r') as f:
        packages = json.load(f)
    assert 'internal' in packages
    location = sys.argv[1]
    project = sys.argv[2]
    command = sys.argv[3]
    dispatch(location, project, command)


if __name__ == '__main__':
    main()
