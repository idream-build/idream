# SEE ALSO
# https://github.com/tweag/rules_haskell
# https://github.com/bazelbuild/rules_scala
# https://github.com/avibryant/bazel-scala-example
# https://docs.bazel.build/versions/master/skylark/rules.html
# https://docs.bazel.build/versions/master/skylark/cookbook.html


def _idris_impl(ctx):
  print(ctx)
  args = ['idris']
  if ctx.attr._is_binary:
    args.extend(['-o', ctx.outputs.executable.path])
  for dep in ctx.attr.deps:
    args.extend(['--package', dep.name])
  for src in ctx.files.srcs:
    args.append('{}/{}'.format(ctx.label.workspace_root, src.path))
  print(args)
  ctx.actions.run_shell(
    mnemonic='Idris',
    outputs=[ctx.outputs.executable],
    # command=' '.join(args),
    command='echo $(pwd) && echo $(ls)',
    env={'HOME':'.'}
  )


def _idris_rule(is_binary):
  return rule(
    implementation = _idris_impl,
    attrs = {
      'srcs': attr.label_list(mandatory=True, allow_files=True),
      'deps': attr.label_list(),
      '_is_binary': attr.bool(default=is_binary),
    },
    executable = is_binary,
  )


idris_binary = _idris_rule(is_binary=True)


idris_library = _idris_rule(is_binary=False)


def _idris_ipkg_library_impl(ctx):
  ctx.actions.run_shell(
    outputs=[],
    command='idris --build {}.pkg'.format(ctx.attr.name),
    env={'HOME':'.'}
  )

idris_ipkg_library = rule(
  implementation = _idris_ipkg_library_impl
)
