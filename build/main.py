#!/usr/bin/env python3

import json
import os
import subprocess
import sys


_ROOT = os.path.dirname(os.path.dirname(__file__))


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


def idris(project, command):
    output_path = os.path.join(_ROOT, 'output')
    bin_path = os.path.join(_ROOT, 'output', 'bin')
    project_output_path = os.path.join(output_path, 'lib', project)
    os.makedirs(project_output_path, exist_ok=True)
    orig_idrispath = subprocess.run(['idris', '--libdir'], stdout=subprocess.PIPE).stdout.decode('utf-8').strip()
    package = read_package(project)
    idrispaths = []
    for pkg in ['base', 'prelude']:
        idrispaths.append(os.path.join(orig_idrispath, pkg))
    for pkg in package.get('pkgs', []):
        idrispaths.append('../../output/{}'.format(pkg))
    idrispaths_arg = ' '.join('--idrispath {}'.format(p) for p in idrispaths)
    idris = 'cd internal/{0} && idris --ibcsubdir ../../output/{0} {2} --{1} {0}.ipkg'.format(
        project, command, idrispaths_arg)
    system(idris)


def dispatch(project):
    project_path = os.path.join(_ROOT, project)
    command = sys.argv[2]
    if command == 'generate':
        generate(project)
    elif command == 'build':
        idris(project, 'build')
    elif command == 'execute':
        idris(project, 'execute')
    # elif command == 'fetch':
    #     fetch(project)
    else:
        raise Exception('Unknown command', command)


def main():
    packages_path = os.path.join(_ROOT, 'packages.json')
    with open(packages_path, 'r') as f:
        packages = json.load(f)
    assert 'internal' in packages
    project = sys.argv[1]
    if project == 'all':
        for project in packages['internal']:
            dispatch(project)
    else:
        dispatch(project)


if __name__ == '__main__':
    main()
