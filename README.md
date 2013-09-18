multishake
==========

A Shake build system that builds a cabal project together with a set of dependencies available in source.

See `Build.hs`.

Note that this is fleshed from something slightly custom (you probably want to read/modify it for your own purposes), but demonstrates some Shake capabilities quite well.

To use it, remove the parts related to `cabalgen` (you probably don't want it) and drop it next to your `.cabal` file.

Built at [Tsuru Capital](http://www.tsurucapital.com/).
