# Syncing with Ruby's faker

Note that whenever we update the submodule, we may want to understand
the changes that went through there. Right now we are only interested
in two of the changes:
* Addition/Deletion of any yml files
* Update in any of the existing yml files

``` shellsession
$ cd scripts
$ ./changes.sh > current.dat
```

Now update the submodule (probably best to do it in a new branch)

``` shellsession
$ git submodule update --remote --merge
$ cd scripts
$ ./changes.sh > now.dat
$ diff current.dat now.dat > diff.dat
$ more diff.dat
```

A sample output from the above `diff` command:

``` shellsession
$ diff current.dat now.dat | more
1d0
< 9fe593332eb04af7e14e1a5074b7e71835d93440020b9e4882646dfdfee3dd7d  arm.yml
13c12
< fbe2b51584e443014e25e46eb8d3837a43ac35abffdec9b2a0ee1ff8f478b16c  ./en/animal.yml
---
> ac670467918a372b1f702fe86ca3d4249df678ead069f9445b50538b49024c8a  ./en/animal.yml
19c18
```

This shows that `arm.yml` file has been deleted and `animal.yml` file
has been modified. These are the rules for knowing what happened:
* Only the presence of `<` for a particular file indicates that it has
  been deleted.
* Only the presence of `>` for a particular file indicates that it has
  been added.
* Presence of both `<` & `>` for a particular file indicates that it
  has been modified.

In the above output, you know that `animal.yml` has been modified. To
know exactly what has changed since the last release, follow these:

``` shellsession
$ cd scripts
$ stack unpack fakedata-0.2.2
```

Now you may want to change the `ymlDiff.sh`'s `OLD_FAKER_DIR` variable.

``` shellsession
$ ./ymlDiff.sh en/animal.yml
```

# Adding support for new yml file

Each Entity (like Book, Address etc.) when added as a new yml file in
the Ruby's faker project as to be exposed in the fakedata
library. There are two modules which needs to be exposed:

* Faker.Provider.\<Entity\>: This module will contain the parsing
  logic of that yaml file.
* Faker.\<Entity\>: This module will actually expose the function
  which will be consumed by the end user.

An easy way of generating those modules is by editing the
[ModuleInfo.hs](./scripts/ModuleInfo.hs) module and then running
`run.sh` script. This will generate the two modules. Note that this
module isn't perfect and you may have to do some alterations (but it
get's 90% of the job done!).

# Custom Fake source support with yml file

You can see the example of `finance.yml` which has been added. Things
that needs to be done:
* Add the yml file into the `customFakeSource` directory.
* Change `Config.hs` appropriately.
* Generate Provider and Faker module
* Profit!

## Template Haskell code

fakedata uses TH extensively to reduce code duplication. To understand
the TH based code, you can read the plain haskell code equivalent. I
have the module `Faker.Address` for easy understanding.

## Benchmark

### fakedata version: 0.2.2

benchmarked God benchmark/single god
time                 160.7 μs   (160.1 μs .. 161.4 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 159.9 μs   (159.4 μs .. 160.6 μs)
std dev              1.951 μs   (1.444 μs .. 2.706 μs)

benchmarking God benchmark/thousand gods ... took 9.288 s, total 56 iterations
benchmarked God benchmark/thousand gods
time                 171.1 ms   (166.1 ms .. 174.5 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 168.3 ms   (166.9 ms .. 169.7 ms)
std dev              2.200 ms   (1.445 ms .. 3.662 ms)

benchmarked Person benchmark/single person
time                 75.17 ms   (74.04 ms .. 76.01 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 77.05 ms   (76.34 ms .. 78.35 ms)
std dev              1.610 ms   (794.0 μs .. 2.732 ms)

benchmarking Person benchmark/10 persons ... took 49.42 s, total 56 iterations
benchmarked Person benchmark/10 persons
time                 905.0 ms   (884.2 ms .. 921.1 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 897.7 ms   (892.4 ms .. 904.5 ms)
std dev              10.22 ms   (7.192 ms .. 13.69 ms)

----------------------------------

Only doing file bytestring benchmarks:

benchmarked God benchmark/single god
time                 173.9 μs   (172.4 μs .. 175.9 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 170.7 μs   (170.1 μs .. 171.6 μs)
std dev              2.573 μs   (2.061 μs .. 3.303 μs)

benchmarking God benchmark/thousand gods ... took 6.991 s, total 56 iterations
benchmarked God benchmark/thousand gods
time                 127.4 ms   (126.8 ms .. 127.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 127.0 ms   (126.7 ms .. 127.2 ms)
std dev              472.1 μs   (331.2 μs .. 697.7 μs)

benchmarked Person benchmark/single person
time                 77.99 ms   (76.92 ms .. 79.74 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 74.97 ms   (73.67 ms .. 76.18 ms)
std dev              2.299 ms   (1.859 ms .. 2.717 ms)

benchmarking Person benchmark/10 persons ... took 48.28 s, total 56 iterations
benchmarked Person benchmark/10 persons
time                 878.0 ms   (861.7 ms .. 913.2 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 877.1 ms   (870.4 ms .. 887.8 ms)
std dev              15.08 ms   (9.567 ms .. 23.57 ms)

--------------------------------------------
Only doing file yaml  benchmarks:

benchmarked God benchmark/single god
time                 166.0 μs   (165.7 μs .. 166.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 164.4 μs   (164.1 μs .. 164.7 μs)
std dev              1.134 μs   (950.7 ns .. 1.449 μs)

benchmarked God benchmark/thousand gods
time                 1.345 ms   (1.342 ms .. 1.347 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.337 ms   (1.335 ms .. 1.339 ms)
std dev              6.066 μs   (5.098 μs .. 7.473 μs)

benchmarked Person benchmark/single person
time                 13.13 ms   (12.73 ms .. 13.56 ms)
                     0.997 R²   (0.995 R² .. 1.000 R²)
mean                 12.75 ms   (12.69 ms .. 12.89 ms)
std dev              235.8 μs   (70.84 μs .. 386.1 μs)

benchmarked Person benchmark/10 persons
time                 24.07 ms   (23.76 ms .. 24.45 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 24.61 ms   (24.46 ms .. 24.78 ms)
std dev              371.9 μs   (270.4 μs .. 507.5 μs)

-----------------------------------------

Doing both yaml and field based caching


# Todo

* In provider functions, change all the resolve*Field function to use cached version.
* Also In provider functions, make the resolve*Text functions spread out stdgen.
* Cleanup Faker module and it's haddock

