xson
---

Xson is a fast JSON parser designed for use with streaming libraries
such as `pipes` and `conduit`. The parser is resumable, allowing you
to stream input in chunks, which of course can be used with lazy IO
and lazy `ByteString`.

The `FromJSON` idiom is modelled with the `Consumer` types from
`pipes` and `conduit`. The type consumes JSON tokens and returns a
parsed result. Streaming tokens rather than converting `Value` allows
an instance of `FromJSON` to bypass the need for any intermediate
representation. In the presence of streaming, this can save an awful
lot of time and memory.

For the case of parsing `Value` specifically, the `pipes` and
`conduit` implementations are nearly identical in performance, and
very close to as fast as `aeson`. But the raw state machine parser in
`Data.Xson` is the fastest, at around 20% faster than `aeson`. All
three of the `xson` options use considerably less memory than `aeson`.

*(Sidenote: I was surprised to see that the state monad implementation
 and the `ST` monad implementation of the raw state machine are nearly
 equivalent in performance.)*

Contributing
---

This project can be built with either Nix or Stack. I previously tried
to unify them and have Stack use the Nix configuration for Haskell
dependencies. This worked, but requires contributors to be using
Nix. In the end, I decided it was a better tradeoff to offer both, but
have them completely disjoint. As such, Stack will not use Nix, and
Nix will not use Stack. If you don't have or know Nix, no worries;
it's not requried. Building is done as you would expect, using either
`stack build` in the root directory, or `nix-build` in any of the
package directories.

I use [brittany](https://github.com/lspitzner/brittany) for automated
formatting. It's fairly bleeding edge, so don't worry about it. I
periodically run `brittany` on all the `.hs` files myself.

Benchmarks
---

*TODO: Emit Criterion's generated graphs.*

This was tested in a VirtualBox VM on my 2013 MacBook Pro, 2.3 GHz
Intel Core i7, 16 GB 1600 MHz DDR3. As test cases, I'm using two files
of JSON info on sets from the card game Magic, The Gathering. One is
for a single set (coded "AER"), and the other is all sets in Magic's
history and therefore much larger. These files don't represent the
most diverse input, and need to be replaced with something more
informative. But they do have plenty of escape characters, some
unicode, and all of the types of JSON values at various nested
levels. All of the different parsing methods are tested against both
of these files in both Criterion and Weigh.

There is also a benchmark for the weird
[issue with continuous slashes](http://www.serpentine.com/blog/2015/05/13/sometimes-the-old-ways-are-the-best/),
but `xson` currently has no workaround for that.

# Criterion

```
benchmarking xson-aer
time                 19.24 ms   (18.83 ms .. 19.69 ms)
                     0.995 R²   (0.987 R² .. 0.999 R²)
mean                 19.44 ms   (19.18 ms .. 19.95 ms)
std dev              887.4 μs   (573.6 μs .. 1.212 ms)
variance introduced by outliers: 17% (moderately inflated)
             
benchmarking xson-aer-ST
time                 18.78 ms   (16.90 ms .. 20.57 ms)
                     0.957 R²   (0.916 R² .. 0.985 R²)
mean                 23.72 ms   (21.67 ms .. 27.94 ms)
std dev              6.289 ms   (3.430 ms .. 10.22 ms)
variance introduced by outliers: 84% (severely inflated)
             
benchmarking xson-aer-pipes
time                 23.66 ms   (22.25 ms .. 25.21 ms)
                     0.976 R²   (0.942 R² .. 0.997 R²)
mean                 25.05 ms   (24.25 ms .. 26.18 ms)
std dev              2.072 ms   (1.234 ms .. 2.980 ms)
variance introduced by outliers: 35% (moderately inflated)
             
benchmarking xson-aer-conduit
time                 24.87 ms   (24.45 ms .. 25.29 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 24.64 ms   (24.47 ms .. 24.81 ms)
std dev              409.9 μs   (298.3 μs .. 540.5 μs)
             
benchmarking aeson-aer
time                 25.20 ms   (24.67 ms .. 25.71 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 25.70 ms   (25.48 ms .. 26.12 ms)
std dev              668.5 μs   (406.8 μs .. 1.058 ms)
             
benchmarking xson-allSets
time                 3.130 s    (2.804 s .. NaN s)
                     0.999 R²   (0.995 R² .. 1.000 R²)
mean                 3.160 s    (3.101 s .. 3.195 s)
std dev              54.06 ms   (0.0 s .. 61.15 ms)
variance introduced by outliers: 19% (moderately inflated)
             
benchmarking xson-allSets-ST
time                 3.179 s    (3.012 s .. 3.308 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 3.278 s    (3.261 s .. 3.288 s)
std dev              14.90 ms   (0.0 s .. 16.78 ms)
variance introduced by outliers: 19% (moderately inflated)
             
benchmarking xson-allSets-pipes
time                 3.739 s    (3.369 s .. 3.960 s)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 3.811 s    (3.743 s .. 3.853 s)
std dev              63.14 ms   (0.0 s .. 72.05 ms)
variance introduced by outliers: 19% (moderately inflated)
             
benchmarking xson-allSets-conduit
time                 3.973 s    (3.891 s .. 4.132 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.006 s    (3.973 s .. 4.029 s)
std dev              35.45 ms   (0.0 s .. 40.61 ms)
variance introduced by outliers: 19% (moderately inflated)
             
benchmarking aeson-allSets
time                 3.704 s    (3.563 s .. 4.019 s)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 3.816 s    (3.741 s .. 3.873 s)
std dev              87.35 ms   (0.0 s .. 98.72 ms)
variance introduced by outliers: 19% (moderately inflated)
             
benchmarking xson-slash
time                 243.5 ms   (214.3 ms .. 266.3 ms)
                     0.997 R²   (0.992 R² .. 1.000 R²)
mean                 250.1 ms   (244.1 ms .. 258.0 ms)
std dev              7.708 ms   (3.257 ms .. 10.08 ms)
variance introduced by outliers: 16% (moderately inflated)
             
benchmarking xson-slash-ST
time                 266.0 ms   (242.0 ms .. 292.1 ms)
                     0.997 R²   (0.988 R² .. 1.000 R²)
mean                 261.2 ms   (256.1 ms .. 267.6 ms)
std dev              7.313 ms   (2.647 ms .. 9.089 ms)
variance introduced by outliers: 16% (moderately inflated)
             
benchmarking xson-slash-pipes
time                 258.1 ms   (235.8 ms .. 280.0 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 262.4 ms   (259.0 ms .. 266.1 ms)
std dev              4.610 ms   (1.295 ms .. 6.243 ms)
variance introduced by outliers: 16% (moderately inflated)
             
benchmarking xson-slash-conduit
time                 259.5 ms   (213.0 ms .. 285.9 ms)
                     0.987 R²   (0.926 R² .. 1.000 R²)
mean                 277.0 ms   (263.4 ms .. 301.4 ms)
std dev              24.78 ms   (871.9 μs .. 29.96 ms)
variance introduced by outliers: 18% (moderately inflated)
             
benchmarking aeson-slash
time                 374.0 μs   (371.4 μs .. 376.8 μs)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 376.1 μs   (372.9 μs .. 384.2 μs)
std dev              15.91 μs   (6.096 μs .. 31.53 μs)
variance introduced by outliers: 37% (moderately inflated)
```

# Weigh

```
Case                          Bytes     GCs  Check
xson-aer                 14,764,664      26  OK   
xson-aer-ST              14,764,600      26  OK   
xson-aer-pipes           24,305,272      45  OK   
xson-aer-conduit         24,171,432      44  OK   
aeson-aer                38,654,760      74  OK   
xson-allSets          2,106,259,328   3,886  OK   
xson-allSets-ST       2,106,234,976   3,886  OK   
xson-allSets-pipes    3,577,209,208   6,747  OK   
xson-allSets-conduit  3,556,536,424   6,695  OK   
aeson-allSets         5,921,648,776  11,282  OK   
xson-slash            1,266,356,128   2,335  OK   
xson-slash-ST         1,266,355,176   2,335  OK   
xson-slash-pipes      1,266,363,952   2,335  OK   
xson-slash-conduit    1,266,359,008   2,335  OK   
aeson-slash               5,495,488       9  OK   
```
