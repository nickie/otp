#!/usr/bin/env python

import re

print_all_gc = False
print_all_mbuf = False

pending = {}

def store (pid, id, data):
    try:
        entry = pending[pid]
    except KeyError:
        entry = { 'last': 0 }
    last = entry['last']
    if id == 0:
        if pending and print_all_gc:
            print "[pid={0}] GC invoked while {1} was/were pending".format(pid, pending.keys())
        if last != 0:
            print "[pid={0}] GC invoked while in state={1}".format(pid, last)
    elif id == 8:
        if pending:
            print "[pid={0}] new message buffer while {1} was/were pending".format(pid, pending.keys())
        elif print_all_mbuf:
            print "[pid={0}] new message buffer with no pending copies".format(pid)
    else:
        if pending and id == 1:
            print "[pid={0}] copy invoked while {1} was/were pending".format(pid, pending.keys())
        if id != last + 1:
            print "[pid={0}] event out of order, {1} after {2}".format(pid, id, last)
        if id == 1:
            entry['original'] = data
        elif id == 3:
            entry['size'] = data
        elif id == 6:
            if entry['original'] != data:
                print "[pid={0}] mismatch:\n  original before = {1}\noriginal after =  {2}".format(entry['original'], data)
            entry['original-after'] = data
        elif id == 7:
            if entry['original'] != data:
                print "[pid={0}] mismatch:\n  original before = {1}\ncopy after =      {2}".format(entry['original'], data)
            if entry['original-after'] != data:
                print "[pid={0}] mismatch:\n  original before = {1}\ncopy after =      {2}".format(entry['original-after'], data)
        if id < 7:
            entry['last'] = id
            pending[pid] = entry
        else:
            del pending[pid]

def parse_message(msg):
    if msg.startswith("message is"):
        return (1, msg[11:])
    elif msg.startswith("calc size"):
        return (2, None)
    elif msg.startswith("size was:"):
        return (3, int(msg[10:]))
    elif msg.startswith("before copy"):
        return (4, None)
    elif msg.startswith("after copy"):
        return (5, None)
    elif msg.startswith("original is"):
        return (6, msg[12:])
    elif msg.startswith("copy is"):
        return (7, msg[8:])
    elif msg.startswith("new message buffer"):
        return (8, None)
    elif msg.startswith("MINOR GC"):
        return (0, None)
    elif msg.startswith("MAJOR GC"):
        return (0, None)
    else:
        print "I don't know what this is:", msg

with open("SEGV", "rt") as f:
    for line in f:
        re_line = re.compile("(.*)\[pid=([^]]+)\] (.*)\n")
        g = re_line.match(line)
        try:
            prf = g.group(1)
            if prf != "":
                print "Ignoring prefix:", prf
            pid = g.group(2)
            (id, data) = parse_message(g.group(3))
            store(pid, id, data)
        except:
            print "Ignoring line:", line.strip()
