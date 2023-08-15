FROM ocaml/opam:debian-11-ocaml-5.2

ENV SHELL=/bin/bash

USER root

RUN apt update && apt-get install -y
# && rm -rf /var/lib/apt/lists/*

USER opam

# opam init -a
RUN opam init -y
RUN opam update
RUN eval `opam env`

# opam switch list-available
RUN opam switch create 5.0.0
RUN eval `opam env`

RUN opam install -y \
  dune \
  utop \
  merlin \
  containers \
  alcotest \
  ocaml-lsp-server

#RUN dune build --watch --terminal-persistence=clear-on-rebuild

WORKDIR /home/opam
