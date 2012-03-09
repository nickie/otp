#!/bin/sh

# ./otp_build autoconf
# ./configure
make -j16 || exit 1
cd erts/emulator
env ERL_TOP=/home/nickie/Projects/release/otp make -j16 debug FLAVOR=plain
env ERL_TOP=/home/nickie/Projects/release/otp make -j16 debug FLAVOR=smp
cd ../..
