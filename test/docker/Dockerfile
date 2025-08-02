ARG VERSION=22.04
FROM ubuntu:$VERSION

RUN apt-get -y update && \
    DEBIAN_FRONTEND=noninteractive apt-get -y install \
      git ruby ruby-colorize python3 python3-colorama python3-nox lcov \
      libxml2-utils tcl tclsh tcllib automake flex llvm-dev check libdw-dev \
      libffi-dev libreadline-dev tcl8.6-dev libzstd-dev libxml2-utils \
      libjansson-dev build-essential dpkg-dev tcl-dev pkg-config debhelper \
      curl wget sudo lsb-release software-properties-common gnupg && \
    apt-get clean all

ENV LC_ALL=C.UTF-8
