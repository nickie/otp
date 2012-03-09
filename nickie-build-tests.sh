#!/bin/sh

env ERL_TOP=/home/nickie/Projects/release/otp \
    ERL_LIBS=/home/nickie/Projects/release/otp/lib \
  ./otp_build tests
