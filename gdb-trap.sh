#!/bin/sh

( cd the_tests_nickie/compiler_test;
  env PATH=/home/nickie/Projects/release/otp/bin:$PATH \
      ERL_TOP=/home/nickie/Projects/release/otp \
    erlc +inline bs_bincomp_SUITE.erl & ) &&
./gdb.py

exit 0

( cd lib/megaco/src/binary;
  env PATH=/home/nickie/Projects/release/otp/bin:$PATH \
      ERL_TOP=/home/nickie/Projects/release/otp \
    erlc -W -bber +noobj megaco_ber_media_gateway_control_v1.set.asn & ) &&
./gdb.py
