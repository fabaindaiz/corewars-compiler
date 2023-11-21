FROM ocaml/opam:latest

SHELL ["/bin/bash", "-c"]

USER root

# Upgrade apt repository packages & Install baseline packages
RUN apt-get update && \
  DEBIAN_FRONTEND="noninteractive" apt-get upgrade --yes
  DEBIAN_FRONTEND="noninteractive" apt-get install --yes \
    nasm \
    clang
# && rm -rf /var/lib/apt/lists/*

# Make typing unicode characters in the terminal work.
ENV LANG en_US.UTF-8

USER opam

# Run user commands

RUN opam init -y && \
    opam update && \
    eval `opam env`

RUN opam install --unlock-base --yes \
    dune \
    utop \
    merlin \
    containers \
    alcotest \
    ocaml-lsp-server

#RUN dune build --watch --terminal-persistence=clear-on-rebuild

WORKDIR /home/opam