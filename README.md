# MLDonkey: cross-platform multi-network peer-to-peer daemon

A lot of documentation (wiki) and user forums were previously hosted at http://mldonkey.sourceforge.net, but were shut down on August 21, 2023,
see [issue #90](https://github.com/ygrek/mldonkey/issues/90) for the details and links to the data dumps (help needed to restore them to the usable form).

[GitHub Discussions](https://github.com/ygrek/mldonkey/discussions) serves as a replacement for the forum for the time being.

Build
=====

Supported OCaml versions are >= 4.14 and < 5.0, see mldonkey.opam for canonical information.

GTK2 GUI is not supported anymore. Only Linux is supported.

Build can be run using dune:

```
python autoconf.py
opam exec -- dune build
```

Dependencies must already be in place.