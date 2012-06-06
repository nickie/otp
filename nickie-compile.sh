#!/bin/sh

CORES=16
ERL_TOP=$PWD

# ./otp_build autoconf
# ./configure
make -j$CORES || exit 1
cd erts/emulator
env ERL_TOP=$ERL_TOP make -j$CORES debug FLAVOR=plain
env ERL_TOP=$ERL_TOP make -j$CORES debug FLAVOR=smp
cd ../..
