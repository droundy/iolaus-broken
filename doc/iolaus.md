Iolaus commands are called with the form

    iolaus command OPTIONS

You may use any unique prefix for `command`, so `iolaus w` is as good
as `iolaus whatsnew`.  The order of the options does not in general
matter.  The various Iolaus commands are listed (and documented)
below.

### Setting defaults

You can set a default flag for a command using

    iolaus command --config-default --test --verbose

which will make `iolaus command` default to using the `--test` and
`--verbose` flags.  These may be overridden on the command line for
flags that have opposites, such as `--no-test`.
