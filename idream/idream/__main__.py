import argparse
from collections import namedtuple, OrderedDict
import json
import logging
import os
import shutil
import subprocess
import time


_LOGGER = logging.getLogger(__name__)


_CACHE = '.idream'
_BIN = 'bin'
_LIB = 'lib'
_EXT = 'ext'


_IDR_PACKAGE_JSON = 'idr-package.json'
_IDR_PACKAGE_SET_JSON = 'idr-package-set.json'
_IDR_PROJECT_JSON = 'idr-project.json'


_IDRIS_COMMANDS = set(['build', 'install', 'repl', 'clean', 'mkdoc',
                       'installdoc', 'checkpkg', 'testpkg'])


def system(command, cwd=None):
    _LOGGER.debug('+ %s', command)
    subprocess.call(command, shell=True, cwd=cwd)


# TODO: migrate
# def idris(location, project, command, package):
#     pkgs = package.get('pkgs', [])
#     executable = package.get('executable', False)
#
#     project_path = os.path.join(_ROOT, location, project)
#     output_path = os.path.join(_ROOT, 'output')
#     bin_path = os.path.join(output_path, 'bin')
#     lib_path = os.path.join(output_path, 'lib')
#     project_lib_path = os.path.join(lib_path, project)
#     os.makedirs(bin_path, exist_ok=True)
#     os.makedirs(project_lib_path, exist_ok=True)
#
#     orig_idrispath = subprocess.run(['idris', '--libdir'], stdout=subprocess.PIPE).stdout.decode('utf-8').strip()
#     idrispaths = []
#     for pkg in os.listdir(orig_idrispath):
#         idrispaths.append(os.path.join(orig_idrispath, pkg))
#     for pkg in pkgs:
#         pkg_output_path = os.path.join(output_path, 'lib', pkg)
#         idrispaths.append(pkg_output_path)
#     idrispaths_arg = ' '.join('--idrispath {}'.format(p) for p in idrispaths)
#
#     idris = 'idris --ibcsubdir {3} {2} --verbose --{1} {0}.ipkg'.format(
#         project, command, idrispaths_arg, project_lib_path)
#     system(idris, cwd=project_path)
#
#     generated_bin = os.path.join(project_path, project)
#     if executable and os.path.isfile(generated_bin):
#         os.rename(generated_bin, os.path.join(bin_path, project))


def make_parser():
    parser = argparse.ArgumentParser(description='A simple build system for Idris')
    parser.add_argument('--cache', type=str, default=None, help='cache location')
    parser.add_argument('--project', type=str, default=None, help='project definitions')
    parser.add_argument('--log-level', type=str, default=None, help='log level')
    subparsers = parser.add_subparsers(dest='op')
    subparsers.add_parser('nuke', help='remove cache')
    subparsers.add_parser('validate', help='validate configuration')
    generate_parser = subparsers.add_parser('generate', help='generate ipkg')
    generate_parser.add_argument('name', type=str, help='name in local packages')
    fetch_parser = subparsers.add_parser('fetch', help='fetch an external dependency')
    fetch_parser.add_argument('name', type=str, help='name in package set')
    return parser


Paths = namedtuple('Paths', [
    'root_path',
    'project_path',
    'cache_path',
    'local_package_set_path',
    'remote_package_set_path',
    'cache_bin_path',
    'cache_lib_path',
    'cache_ext_path'
])


def make_paths(project, cache):
    cwd = os.path.abspath(os.getcwd())
    if project is None:
        project_path = os.path.join(cwd, _IDR_PROJECT_JSON)
        root_path = cwd
    else:
        project_path = os.path.abspath(project)
        root_path = os.path.dirname(project_path)
    if cache is None:
        cache_path = os.path.join(root_path, _CACHE)
    else:
        cache_path = os.path.abspath(cache)
    local_package_set_path = os.path.join(root_path, _IDR_PACKAGE_SET_JSON)
    remote_package_set_path = os.path.join(cache_path, _IDR_PACKAGE_SET_JSON)
    cache_bin_path = os.path.join(cache_path, _BIN)
    cache_lib_path = os.path.join(cache_path, _LIB)
    cache_ext_path = os.path.join(cache_path, _EXT)
    return Paths(
        root_path=root_path,
        project_path=project_path,
        cache_path=cache_path,
        local_package_set_path=local_package_set_path,
        remote_package_set_path=remote_package_set_path,
        cache_bin_path=cache_bin_path,
        cache_lib_path=cache_lib_path,
        cache_ext_path=cache_ext_path)


def init_paths(paths):
    assert os.path.isdir(paths.root_path)
    assert os.path.isfile(paths.project_path)
    os.makedirs(paths.cache_bin_path, exist_ok=True)
    os.makedirs(paths.cache_lib_path, exist_ok=True)
    os.makedirs(paths.cache_ext_path, exist_ok=True)


PackagePair = namedtuple('PackagePair', [
    'package_root',
    'package'
])


Context = namedtuple('Context', [
    'project',
    'packages'
])


def load_context(paths):
    with open(paths.project_path, 'r') as f:
        project = json.load(f)
    packages = OrderedDict()
    rel_paths = project.get('packages', ['.'])
    for rel_path in rel_paths:
        package_root = os.path.join(paths.root_path, rel_path)
        package_path = os.path.join(package_root, _IDR_PACKAGE_JSON)
        with open(package_path, 'r') as f:
            package = json.load(f)
        name = package['name']
        pair = PackagePair(package_root=package_root, package=package)
        if name in packages:
            raise Exception('Duplicate package name', name)
        packages[name] = pair
    return Context(project=project, packages=packages)


def has_local_package_set(paths):
    return os.path.isfile(paths.local_package_set_path)


def load_local_package_set(paths):
    with open(paths.local_package_set_path, 'r') as f:
        return json.load(f)


def resolve_package_set(paths):
    if not has_local_package_set(paths):
        raise Exception('TODO support remote package sets')
    package_set = load_local_package_set(paths)
    _LOGGER.debug('loaded package set %s', package_set)
    return package_set


def assert_can_find_packages(all_names, name, pkgs):
    for pkg in pkgs:
        if pkg not in all_names:
            raise Exception('Unknown pkg for', name, pkg)


def validate(paths, context):
    # TODO validate project JSON and package JSONs
    package_set = resolve_package_set(paths)
    # TODO validate package_set JSON
    all_names = set(context.packages.keys())
    all_names.update(package_set.keys())
    for (name, package_pair) in context.packages.items():
        pkgs = package_pair.package.get('pkgs', [])
        assert_can_find_packages(all_names, name, pkgs)
    for (name, ext_package) in package_set.items():
        pkgs = ext_package.get('pkgs', [])
        assert_can_find_packages(all_names, name, pkgs)


def make_project_ext_path(paths, name):
    return os.path.join(paths.cache_ext_path, name)


def fetch(paths, context, name):
    package_set = resolve_package_set(paths)
    package = package_set[name]
    repo = package['repo']
    version = package['version']
    project_ext_path = make_project_ext_path(paths, name)
    if not os.path.isdir(project_ext_path):
        system('git clone {}'.format(repo), cwd=paths.cache_ext_path)
        system('git checkout {}'.format(version), cwd=project_ext_path)
    else:
        _LOGGER.debug('skipping %s', name)


def generate(paths, context, name):
    package_pair = context.packages[name]

    ipkg_path = os.path.join(package_pair.package_root, '{}.ipkg'.format(name))

    package = package_pair.package
    sourcedir = package.get('sourcedir')
    modules = package.get('modules', [])
    pkgs = package.get('pkgs', [])
    # TODO migrate executable attribute to match ipkg
    executable = package.get('executable', False)

    _LOGGER.debug('generating %s', ipkg_path)

    with open(ipkg_path, 'w') as f:
        f.write('-- NOTE: This file was autogenerated by idream. Do not edit.\n')
        f.write('package {}\n'.format(name))
        if sourcedir is not None:
            f.write('sourcedir = {}\n'.format(sourcedir))
        if modules:
            f.write('modules = {}\n'.format(', '.join(modules)))
        if executable:
            f.write('executable = {}\n'.format(name))
            f.write('main = Main\n')
        if pkgs:
            f.write('pkgs = {}\n'.format(', '.join(pkgs)))


def dispatch(args, paths, context):
    if args.op == 'validate':
        validate(paths, context)
    elif args.op == 'fetch':
        fetch(paths, context, args.name)
    elif args.op == 'generate':
        generate(paths, context, args.name)
    elif args.op in _IDRIS_COMMANDS:
        raise Exception('TODO')
    else:
        raise Exception('Unknown op', args.op)


def main():
    start = time.time()
    parser = make_parser()
    args = parser.parse_args()
    handler = logging.StreamHandler()
    _LOGGER.addHandler(handler)
    if args.log_level is not None:
        _LOGGER.setLevel(args.log_level)
    paths = make_paths(args.project, args.cache)
    _LOGGER.info('loaded paths %s', paths)
    if args.op == 'nuke':
        # Special early handling here to skip loading
        if os.path.isdir(paths.cache_path):
            shutil.rmtree(paths.cache_path)
    else:
        context = load_context(paths)
        _LOGGER.debug('loaded context %s', context)
        init_paths(paths)
        dispatch(args, paths, context)
    end = time.time()
    _LOGGER.debug('completed in %f seconds', end - start)


if __name__ == '__main__':
    main()
