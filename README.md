# MLDonkey: cross-platform multi-network peer-to-peer daemon

## Introduction

This repo is a fork on the official mldonkey software. It includes new features that are still not upstreamed:

* port to **ocaml5** (latest tested is 5.5.0);
* port to the latest **Ubuntu version 26.04**;
* port to dune build system;
* many external libs were extracted from the repo and updated;
* binary protocol was improved;
* builds faster with concurrent jobs;
* can be built against musl, to get a fully static Linux binary.

A lot of documentation (wiki) and user forums were previously hosted at http://mldonkey.sourceforge.net, but were shut down on August 21, 2023,
see [issue #90](https://github.com/ygrek/mldonkey/issues/90) for the details and links to the data dumps (help needed to restore them to the usable form).

[GitHub Discussions](https://github.com/ygrek/mldonkey/discussions) serves as a replacement for the forum for the time being.

## Build

Only ocaml >= 5 is supported, see mldonkey.opam for canonical information.

GTK2 GUI is not supported anymore. Only Linux is supported.

Build can be run using dune:

```
python autoconf.py
opam exec -- dune build
```

Dependencies must already be in place.

## Docker Image

The best way to use mldonkey is to use it in docker. For an updated docker image, which includes all the changes from this mldonkey fork, you can use the [image I provide](https://bugfreeblog.duckdns.org/docker-images-for-the-mldonkey-service). You have many tags available, from the official mldonkey version (which is very outdated), to the most recent version of this repo.

## UI

There are several user interfaces available for mldonkey, but they tend to be a bit outdated. mldonkey includes a powerful web interface, but it is a bit old fashioned as well.

For a modern interface, with a good support for mobile, you can try this app I wrote: [mldonkey-next](https://github.com/carlonluca/mldonkey-next).

It is a bit difficult to set it up, but it is already included in the [docker image I provide](https://bugfreeblog.duckdns.org/docker-images-for-the-mldonkey-service).

mldonkey-next is available for the browser, for [Android and for Linux](https://bugfreeblog.duckdns.org/2024/11/mldonkey-native-app.html).