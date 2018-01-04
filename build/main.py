#!/usr/bin/env python3

import json
import os
import sys


_ROOT = os.path.dirname(os.path.dirname(__file__))


def generate(project):
    package_path = os.path.join(_ROOT, 'internal', project, 'package.json')
    ipkg_path = os.path.join(_ROOT, 'internal', project, '{}.ipkg'.format(project))
    with open(package_path, 'r') as f:
        package = json.load(f)
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
    project_output_path = os.path.join(output_path, project)
    os.makedirs(output_path, exist_ok=True)
    command = 'cd internal/{0} && idris --ibcsubdir output --idrispath ../../output --{1} {0}.ipkg'.format(
        project, command)
    system(command)
    if command == 'build':
        system('ln -s internal/{}/output {}'.format(project, project_output_path))
    elif command == 'clean':
        system('unlink {}'.format(project_output_path))


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
