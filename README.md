# haskell-updater

Rebuilds Haskell packages on Gentoo after a GHC upgrade or a dependency upgrade.

Updating Haskell packages
=========================

Sometimes:

``` shell
emerge -auvDN --keep-going @world
```

has trouble figuring out how to update Haskell packages. Providing emerge
with the full list of dev-haskell packages that have upgrades available can
sometimes help:

``` shell
emerge -avu --oneshot --keep-going --with-bdeps=y @world
haskell-updater -- --verbose-conflicts
```

Sometimes we have sub-slot blockers (when updating ghc or some specific package
there are a list of blockers). Subslot blockers are a portage limitation (bug).

To find solution use larger `--backtrack=` with `emerge` and `haskell-updater`.

Experimental portage invocation
===============================

If you run into errors where `haskell-updater` tries to reinstall a masked or
unavailable package, try `haskell-updater --mode=reinstall-atoms` or
`haskell-updater --world`. If you find any bugs, report them to the [bug
tracker](https://github.com/gentoo-haskell/haskell-updater/issues).


