# haskell-updater

Rebuilds Haskell packages on Gentoo after a GHC upgrade or a dependency upgrade.

Updating Haskell packages
=========================

Sometimes:

``` shell
emerge -auvDN --keep-going @world
```

has trouble figuring out how to update Haskell packages. Providing emerge with the full list of dev-haskell packages that have upgrades available can sometimes help:

``` shell
eix-update
emerge -av --oneshot --keep-going `eix --only-names --upgrade -C dev-haskell`
haskell-updater
```

Unless `EAPI="6"` is approved sometimes we have sub-slot blockers (when updating ghc or some specific package there are a list of blockers) this issue could be solved via running

``` shell
haskell-updater -all -- dev-lang/ghc-7.6.2
```
