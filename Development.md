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

## Populating other-modules in package.yaml

Note that the below `sed` command didn't seem to work with `fish`
shell (But works fine with `bash`).

``` shellsession
$ cd src/Faker/Provider
$ ls | sed "s/^/- Faker.Provider./g" | sed "s/.hs//g"
```

## Template Haskell code

fakedata uses TH extensively to reduce code duplication. To understand
the TH based code, you can read the plain haskell code equivalent. I
have the module `Faker.Address` for easy understanding.

## Debugging using the Ruby Program

``` shellsession
$ cd faker
$ gem build faker.gemspec
$ sudo gem install ./faker-2.3.0.gem
$ irb
irb(main):001:0> require 'faker'
=> true
irb(main):002:0> Faker::Address.city
=> "East Audrea"
irb(main):005:0> Faker::Config.locale = "ar"
=> "ar"
irb(main):006:0> Faker::Address.city
=> "ميناء حاتم"
```

## Known locale issues

These issues are best fixed in the upstream ruby library.

* de-ch: name (wrong/missing data)
* en-ca
  - postcode (regex)
  - formats (wrong/missing data)
* en-gb
  - postcode (regex)
* en-us
  - Whole thing is broken
* en-MX:
  - phone_number.formats, cell_phone.formats is broken
* fr-CA
  - postcode(regex)
* nl
  - postcode (regex)
* vi: postcode (regex)

These are fixes which might be just fixed by some code changes:

* en-NEP: Root key fix
* en-MS: name
* en-NG: fix cities
* en-sg: Fix name
* en-ug: Fix city
* en-za: Fix post_code, company, city
* en-MX: company.name, city, state
* es: company.name
* fr-ca: pokemon, phonenumber
* fr-ch: see why test is failing
* fr: pokemon, compas
* he: name, address
* hy: country_code, address, job, book.author
* id: name
* it: name.suffix
* ja: company.category? should it exist, name
* ko: company
* nb-no: address
* nl: name
* pt-br: team, phonenumber, university
* pt: city
* ru: commerce.color ? address, company.name
* sk: name, address
* sk: commerce.color ? , team
* tr: delete safe_email
* uk: commerce.color ? and fix a few
* vi: company, city
* zh-cn: 
* zh-tw: name, address
