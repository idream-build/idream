# SEE ALSO
# https://github.com/tweag/rules_haskell
# https://github.com/bazelbuild/rules_scala
# https://github.com/avibryant/bazel-scala-example
# https://docs.bazel.build/versions/master/skylark/rules.html
# https://docs.bazel.build/versions/master/skylark/cookbook.html


IdrisPackageInfo = provider(
  doc = 'Package information exposed by Idris libraries.',
  fields = {
    'name': 'Package name'
  }
)


def _idris_binary(ctx):
  pass


idris_binary = rule(
    implementation = _idris_binary,
    attrs = {
        'srcs': attr.label_list(mandatory=True, allow_files=True),
        'deps': attr.label_list(),
    },
)


def _idris_library(ctx):
  pass


idris_library = rule(
    implementation = _idris_library,
    attrs = {
        'srcs': attr.label_list(mandatory=True, allow_files=True),
        'deps': attr.label_list(),
    },
)
