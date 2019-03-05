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
$ diff current.dat now.dat | more
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

This shows that `arm.yml` file has been deleted and `animal.yml` file has been modified. These are the rules for knowing what happened:
* Only the presence of `<` for a particular file indicates that it has been deleted.
* Only the presence of `>` for a particular file indicates that it has been added.
* Presence of both `<` & `>` for a particular file indicates that it has been modified.
