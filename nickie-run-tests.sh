#!/bin/sh

cd release/tests/test_server
env ERL_TOP=/home/nickie/Projects/release/otp \
    ERL_LIBS=/home/nickie/Projects/release/otp/lib \
  /home/nickie/Projects/release/otp/bin/cerl -debug
cd ..
