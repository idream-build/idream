import argparse
from collections import namedtuple, OrderedDict
import json
import jsonschema
import logging
import os
import shutil
import subprocess
import time


_LOGGER = logging.getLogger(__name__)


_SCHEMAS_PATH = os.path.join(os.path.dirname(__file__), 'schemas')


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
    fetch_parser.add_argument('--force-fetch', action='store_true', help='force re-download')
    for command in _IDRIS_COMMANDS:
        command_parser = subparsers.add_parser(command, help='idris {}'.format(command))
        command_parser.add_argument('name', type=str, help='name local packages or package set')
        command_parser.add_argument('--no-generate', action='store_true', help='do not generate ipkg')
        command_parser.add_argument('--no-fetch', action='store_true', help='do not fetch an external dependency')
        command_parser.add_argument('--force-fetch', action='store_true', help='force re-download')
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


def resolve_schemas():
    schemas = {}
    for p in os.listdir(_SCHEMAS_PATH):
        with open(os.path.join(_SCHEMAS_PATH, p), 'r') as f:
            schema = json.load(f)
        schemas[p] = schema
    return schemas


def validate(paths, context):
    schemas = resolve_schemas()
    jsonschema.validate(context.project, schemas[_IDR_PROJECT_JSON])
    for package_pair in context.packages.values():
        jsonschema.validate(package_pair.package, schemas[_IDR_PACKAGE_JSON])
    package_set = resolve_package_set(paths)
    jsonschema.validate(package_set, schemas[_IDR_PACKAGE_SET_JSON])

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


def fetch(paths, context, name, force_fetch):
    package_set = resolve_package_set(paths)
    package = package_set[name]
    repo = package['repo']
    version = package['version']
    project_ext_path = make_project_ext_path(paths, name)
    # TODO support force
    if not os.path.isdir(project_ext_path):
        _LOGGER.debug('fetching %s', name)
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
        f.write('package {}\n'.format(name))
        f.write('-- NOTE: This file was autogenerated by idream. Do not edit.\n')
        if sourcedir is not None:
            f.write('sourcedir = {}\n'.format(sourcedir))
        if modules:
            f.write('modules = {}\n'.format(', '.join(modules)))
        if executable:
            f.write('executable = {}\n'.format(name))
            f.write('main = Main\n')
        if pkgs:
            f.write('pkgs = {}\n'.format(', '.join(pkgs)))


def resolve_package_pair(paths, context, name, no_generate, no_fetch, force_fetch):
    if name in context.packages:
        _LOGGER.debug('resolved local package %s', name)
        if not no_generate:
            generate(paths, context, name)
        return context.packages[name]
    else:
        _LOGGER.debug('resolving remote package %s', name)
        package_set = resolve_package_set(paths)
        if name not in package_set:
            raise Exception('Unknown package', name)
        package = package_set[name]
        project_ext_path = make_project_ext_path(paths, name)
        if not os.isdir(project_ext_path):
            if not no_fetch:
                fetch(paths, context, name, force_fetch)
            else:
                raise Exception('Need to fetch', name)
        else:
            if force_fetch:
                fetch(paths, context, name, force_fetch)
            return PackagePair(package_path=project_ext_path, package=package)


def query_orig_idrispath():
    result = subprocess.run(['idris', '--libdir'], stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8').strip()


def query_builtin_pkgs():
    orig_idrispath = query_orig_idrispath()
    return list(os.path.join(orig_idrispath, pkg) for pkg in os.listdir(orig_idrispath))


def idris(paths, context, name, command, no_generate, no_fetch, force_fetch):
    package_pair = resolve_package_pair(paths, context, name, no_generate, no_fetch, force_fetch)

    package_root = package_pair.package_root
    package = package_pair.package
    pkgs = package.get('pkgs', [])
    # TODO migrate to ipkg format for executable
    executable = package.get('executable', False)

    package_lib = os.path.join(paths.cache_lib_path, name)

    pkg_paths = query_builtin_pkgs()
    for pkg in pkgs:
        path = os.path.join(paths.cache_lib_path, pkg)
        pkg_paths.append(path)

    args = ['idris', '--verbose', '--ibcsubdir', package_lib]
    for path in pkg_paths:
        args.extend(['--idrispath', path])
    args.extend(['--{}'.format(command), '{}.ipkg'.format(name)])

    exe = ' '.join(args)
    system(exe, cwd=package_root)

    generated_bin = os.path.join(package_root, name)
    if executable and os.path.isfile(generated_bin):
        dest = os.path.join(paths.cache_bin_path, name)
        os.rename(generated_bin, dest)


def dispatch(paths, context, args):
    if args.op == 'validate':
        validate(paths, context)
    elif args.op == 'fetch':
        fetch(paths, context, args.name, args.force_fetch)
    elif args.op == 'generate':
        generate(paths, context, args.name)
    elif args.op in _IDRIS_COMMANDS:
        idris(paths, context, args.name, args.op, args.no_generate, args.no_fetch, args.force_fetch)
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
        # Special early handling here to skip context loading and cache creation
        if os.path.isdir(paths.cache_path):
            shutil.rmtree(paths.cache_path)
    else:
        context = load_context(paths)
        _LOGGER.debug('loaded context %s', context)
        init_paths(paths)
        dispatch(paths, context, args)
    end = time.time()
    _LOGGER.debug('completed in %f seconds', end - start)


if __name__ == '__main__':
    main()
