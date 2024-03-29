## v0.8.0 (2022-03-27)

* Rename the freestanding toolchain to solo5 (@dinosaure, #42)
* Use ocamlformat (@samoht, #45)

## v0.7.1 (2022-03-14)

* Do not `Lwt.catch` on the listen callback (@hannesm, @dinosaure, #41)

## v0.7.0 (2022-02-01)

* Upgrade and fix the package with newer version of dependencies (@hannesm, #37)
* Be able to compile & install `mirage-net-solo5` without the expected `dune`'s context (@TheLortex, @dinosaure, #38)
* Use `Solo5_os` instead of `OS` (@dinosaure, #39)

## v0.6.2 (2020-09-04)

* Netif.listen does not catch Out_of_memory anymore (@hannesm, #36)
* Each network interface is now a metrics source reporting statistics
  (see https://github.com/mirage/metrics for further information, @hannesm, #34)

## v0.6.1 (2019-11-01)

* Adapt to mirage-net 3.0.0 changes (@hannesm, #35)
* Raise OCaml lower bound to 4.06.0 (@hannesm, #35)
* cleanup: use Mirage_net.Stats helper functions (@hannesm, #35)

## v0.6.0 (2019-09-25)

* Synchronise version number with other Mirage/Solo5 component packages.
* Update to Solo5 0.6.0+ APIs, multiple devices support (@mato, #32)

## v0.4.3 (2019-07-16)

* Adjust to macaddr version 4.0.0 API (@yomimono, #33)
* Port to Dune (@pascutto, #29)

## v0.4.2 (2019-02-24)

* Adjust to mirage-net 2.0.0 interface changes (@hannesm, #28)

## v0.4.1 (2019-01-07)

* Migrate to ipaddr 3.0 (@hannesm, #27)

## v0.4.0 (2018-11-08)

* Correctly use Cstruct buffer offset (@mato, #25)
* Migrate to OPAM 2 (@mato, #24)
* Remove open Result (@hannesm, #23)

## v0.3.0 (2018-06-17)

* Adapt to Solo5 v0.3.0 API changes, refactor and clean up dead code (@mato,
  #21)

## v0.2.0 (2017-01-17)

* Port to topkg (@hannesm, #10)
* Use Log module for logging (@mato, #8)
* Remove remaining Xen leftovers, fix big writes (@mato, #3)
* Update types and interface for MirageOS 3 (@hannesm, @yomimono, @samoht)

## v0.1.1 (2016-07-15)

* Initial Solo5 release derived from mirage-net-xen.
