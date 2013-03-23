# A tiny Haskell library for timing IO actions.

[![Build Status](https://secure.travis-ci.org/thoughtpolice/hs-microtimer.png?branch=master)](http://travis-ci.org/thoughtpolice/hs-microtimer)

This package contains a very simple module for benchmarking `IO`
actions. It was taken from [criterion][].

The API simply provides:

```haskell
-- | Time an 'IO' action and return the time taken for execution,
-- as well as the return value.
time :: IO a -> IO (Double, a)

-- | Time an 'IO' action, throwing away the result and returning
-- the time taken for execution.
time_ :: IO a -> IO Double

-- | Convert a 'Double' value into a 'String' which specifies
-- how long something took in seconds.
formatSeconds :: Double -> String
```

# Installation

It's just a `cabal install` away on [Hackage][]:

```bash
$ cabal install microtimer
```

# Join in

Be sure to read the [contributing guidelines][contribute]. File bugs
in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/thoughtpolice/hs-microtimer.git`

There's also a [BitBucket mirror][bb]:

* `git clone https://bitbucket.org/thoughtpolice/hs-microtimer.git`

# Authors

See [AUTHORS.txt](https://raw.github.com/thoughtpolice/hs-microtimer/master/AUTHORS.txt).

# License

BSD3. See
[LICENSE.txt](https://raw.github.com/thoughtpolice/hs-microtimer/master/LICENSE.txt)
for terms of copyright and redistribution.

[criterion]: http://hackage.haskell.org/package/criterion
[contribute]: https://github.com/thoughtpolice/hs-microtimer/blob/master/CONTRIBUTING.md
[issue tracker]: http://github.com/thoughtpolice/hs-microtimer/issues
[gh]: http://github.com/thoughtpolice/hs-microtimer
[bb]: http://bitbucket.org/thoughtpolice/hs-microtimer
[Hackage]: http://hackage.haskell.org/package/microtimer
