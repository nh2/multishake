#!/usr/bin/env python2

# cabalgen: Conveniently manage cabal projects with many executables.
#
# - detects modules in src/
# - manages executables with ghc-options and rtsopts
# - warns for unused unitTests

TEMPLATE = 'myproject.cabal.template'

# Modules that shall be excluded from exposed-modules.
SKIP_MODULES = [
]

# Order: Name of executable, library module with main function, ghc-options, rtsopts
EXECUTABLES = [
    ['myexecutable',                'My.Package.Main',                    ['-threaded'], '-N3 -A100M -K30M'],
]

import argparse
import os
import re
import subprocess
import sys

parser = argparse.ArgumentParser(description="Conveniently generates a cabal file")
parser.add_argument('-prof', action='store_true', default=False,
                    help="Enable profiling. Disables -eventlog and uses -debug instead")
parser.add_argument('-dump-executables', dest='dump_executables', action='store_true', default=False,
                    help="Print the names of all executables in cabalgen and the cabal.template, then exit")
args = parser.parse_args()


# Read cabal template
cabal_template = open(TEMPLATE).read()


# Mode for only printing all executable names
if args.dump_executables:
    # cabalgen executables
    for e in EXECUTABLES:
        print e[0]
    # cabal.template executables
    for e in re.findall(r'executable\s+(\S+)', cabal_template):
        print e
    sys.exit()


# Replace a placeholder in the template.
# Placeholders should look like '..._GENERATED_HERE'.
def template_replace(str, repl):
    global cabal_template
    cabal_template = cabal_template.replace(str, repl)

# Run a shell command and split by lines.
def shell_lines(cmd):
    return subprocess.check_output(cmd, shell=True).strip().split()

template_replace("AUTOGENERATION_WARNING_HERE", "-- WARNING: This file is generated and will be overriden by cabalgen. Edit *.cabal.template instead.")


# Modules

print "Crawling modules from source tree"

# Find all modules in *.hsc? files looking for 'module ...'.
modules = shell_lines(
    "find src -regex '[^.].*\.hsc?' | xargs perl -ne 'print if s/^module\s+([\w\.]+).*/$1/g'")

# Error on duplicate modules.
dup_modules = [x for x in modules if modules.count(x) > 1]
if dup_modules:
    print >> sys.stderr, "cabalgen ERROR: duplicate modules: ", dup_modules
    sys.exit(1)

# Warn for skipped modules.
if SKIP_MODULES:
    print "Skipping modules:"
    for m in SKIP_MODULES:
        print "  %s" % m

used_modules = [ m for m in modules if m not in SKIP_MODULES ]


print "Generating cabal modules"

template_replace("MODULES_GENERATED_HERE", '\n    '.join(used_modules))


# Executables

# Format a cabal executable secion.
def format_executable(name, module, ghc_options=[], rtsopts=''):

    def replace_ghc_option(option, repl, reason='no reason given'):
        r = [ repl if o.strip() == option.strip() else o for o in ghc_options ]
        if r != ghc_options:  # we changed something
            print "  Executable %s: Replaced GHC option '%s' by '%s' (%s)" % (name, option, repl, reason)
        return r

    if args.prof:
        # Replace -eventlog by -debug for profiling
        # (you can't have -threaded, -eventlog and -prof toghether,
        # but it works with -debug instead of -eventlog; this is slower).
        ghc_options = replace_ghc_option('-eventlog', '-debug', reason='we are profiling')

    with_rtsopts = '"-with-rtsopts=%s"' % rtsopts if rtsopts else ''
    return """
-- {module}
executable {name}
    hs-source-dirs: gen-executables
    build-depends: myproject, base

    main-is: {name}.hs
    ghc-options: -rtsopts {ghc_options} {with_rtsopts}
""".format(name=name, module=module, ghc_options=' '.join(ghc_options), with_rtsopts=with_rtsopts)


print "Generating cabal executables"

template_replace("EXECUTABLES_GENERATED_HERE",
                 '\n'.join(format_executable(*e) for e in EXECUTABLES))


# Format a stub Main module that just calls another main.
def format_main_stub(module):
    return """
module Main where

import qualified {module} (main)

main :: IO ()
main = {module}.main
""".format(module=module)

# Write contents to a file if the contents have changed
def write_file_if_changed(f, contents, verbose=False):
    if (os.path.exists(f) and contents == open(f).read()):
        if verbose:
            print "Not writing %s (unchanged)" % f
    else:
        if verbose:
            print "Writing %s" % f
        with open(f, "w") as f:
            f.write(contents)

print "Generating executable stubs in gen-executables/"

if not os.path.exists('gen-executables'): os.mkdir('gen-executables')

for e in EXECUTABLES:
    name, module = e[0], e[1]
    exe_stub_code = format_main_stub(module)
    write_file_if_changed("gen-executables/%s.hs" % name, exe_stub_code)


# Write cabal file

# Not writing if unchanged saves us unnecessary cabal configure
write_file_if_changed("myproject.cabal", cabal_template, verbose=True)


# Tests

# Look for 'unitTest ::' to warn for unused tests.
modules_with_unitTests = shell_lines(
    "grep -rIPl '\\bunitTests\s+::' src | xargs perl -ne 'print if s/^module\s+([\w\.]+).*/$1/g'")

tested_modules = shell_lines(
    "perl -ne 'print if s/(.*?)(\S+)\.unitTests.*/$2/g' executables/tests-unit.hs")


untested_modules = [ m for m in modules_with_unitTests if m not in tested_modules ]

if untested_modules:
    print "WARNING: The following modules have unitTests that are not in executables/tests-unit.hs:"
    for m in untested_modules:
        print "  %s" % m
