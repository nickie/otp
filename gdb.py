#!/usr/bin/env python

from subprocess import call, Popen, PIPE

p1 = Popen(["ps", "-ww", "x"], stdout=PIPE)
p2 = Popen(["grep", "beam"], stdin=p1.stdout, stdout=PIPE)
p1.stdout.close()
output = p2.communicate()[0]

for line in output.splitlines():
    words = line.split()
    execf = words[4]
    if execf == "grep":
	continue
    pid = words[0]
    call(["gdb", execf, pid])
    break
