#!/usr/bin/env python

import sys, re

print_all_gc = False

pending = {}

def store (pid, id, data, next, fun):
    try:
        entry = pending[pid]
    except KeyError:
        entry = {'last': 0, 'expecting': [], 'exclusive': False}
    exclusive = entry['exclusive']
    expecting = entry['expecting']
    pending_exclusive = []
    for opid in pending:
        if pid != opid and pending[opid]['exclusive']:
            pending_exclusive.append(opid)
    if id == 0:
        if exclusive:
            print "[pid={0}] GC invoked while in exclusive state {1}".format(pid, last)
        if pending_exclusive and print_all_gc:
            print "[pid={0}] GC invoked while {1} was/were pending in exclusive state".format(pid, pending_exclusive)
    else:
        try:
            next_exclusive = next[0]
            next_expecting = next[1]
        except:
            next_exclusive = exclusive
            next_expecting = []
        try:
            expected = expecting.pop(0)
            if id != expected:
                print "[pid={0}] unexpected event {1}, expected {2}".format(pid, id, expected)
        except:
            if pending_exclusive and next_exclusive:
                print "[pid={0}] event {1} while {2} was/were pending in exclusive state".format(pid, id, pending_exclusive)
        fun(pid, entry, data)
        expecting.extend(next_expecting)
        if expecting:
            entry['last'] = id
            entry['exclusive'] = next_exclusive
            pending[pid] = entry
        else:
            try:
                del pending[pid]
            except:
                pass

def is_obj (ptr):
    return ptr % 4 in [1, 2]

obj = {}

def check_pid (ptr, pid):
    if obj[ptr]['pid']:
        if not pid in obj[ptr]['pid']:
            print "[pid={0}] check_pid shared object {1:x}".format(pid, ptr)
    obj[ptr]['pid'].add(pid)

def fun_nothing (pid, entry, data):
    pass
def fun_gc (pid, entry, data):
    for ptr in obj:
        if pid in obj[ptr]['pid']:
            if len(obj[ptr]['pid'].size) == 1:
                del obj[ptr]
            else:
                obj[ptr]['pid'].remove(pid)
def fun_size_object (pid, entry, data):
    if is_obj(data):
        if data in obj:
            check_pid(data, pid)
        else:
            obj[data] = {'pid': set([pid])}
    entry['original'] = data
def fun_size_was (pid, entry, data):
    ptr = entry['original']
    if is_obj(ptr):
        check_pid(ptr, pid)
        if entry['last'] == 1:
            size_type = 'flat_size'
        elif entry['last'] == 6:
            size_type = 'shared_size'
        else:
            print "[pid={0}] size_was with last event {1}, WTF?".format(pid, entry['last'])
            sys.exit(0)
        try:
            if obj[ptr][size_type] != data:
                print "[pid={0}] {1} differs for object {2:x}".format(pid, size_type, ptr)
                obj[ptr][size_type] = data
        except:
            obj[ptr][size_type] = data
    entry['size'] = data
def fun_copy_struct (pid, entry, data):
    if is_obj(data):
        if data in obj:
            check_pid(data, pid)
        else:
            obj[data] = {'pid': set([pid])}
    entry['original'] = data
def fun_result_is_at (pid, entry, data):
    ptr = entry['original']
    if is_obj(ptr):
        check_pid(ptr, pid)
        if entry['last'] == 3:
            copy_type = 'flat'
        elif entry['last'] == 9:
            copy_type = 'shared'
        else:
            print "[pid={0}] result_is_at with last event {1}, WTF?".format(pid, entry['last'])
            sys.exit(0)
        obj[data] = {'pid': set(), 'copy': ptr, 'copy_type': copy_type}
        try:
            obj[ptr][copy_type] = obj[ptr][copy_type] + 1
        except:
            obj[ptr][copy_type] = 1
def fun_copy_shared_calculate (pid, entry, data):
    if is_obj(data):
        if data in obj:
            check_pid(data, pid)
        else:
            obj[data] = {'pid': set([pid])}
    entry['original'] = data
def fun_message_is (pid, entry, data):
    ptr = entry['original']
    if is_obj(ptr):
        check_pid(ptr, pid)
        try:
            if obj[ptr]['term'] != data:
                print "[pid={0}] term differs for object {1:x}".format(pid, ptr)
                obj[ptr]['term'] = data
        except:
            obj[ptr]['term'] = data
    entry['original_term'] = data
def fun_copy_shared_perform (pid, entry, data):
    ptr = entry['original']
    if is_obj(ptr):
        check_pid(ptr, pid)
    if ptr != data:
        print "[pid={0}] copy_shared_{calculate/perform} mismatch: {1} vs {2}".format(pid, entry['original'], data)
def fun_original_was (pid, entry, data):
    ptr = entry['original']
    if is_obj(ptr):
        check_pid(ptr, pid)
    if entry['original_term'] != data:
        print "[pid={0}] copy_shared_perform mismatch:\n  original before = {1}\noriginal after =  {2}".format(pid, entry['original_term'], data)
    entry['original_term_after'] = data
def fun_copy_is (pid, entry, data):
    ptr = entry['original']
    if is_obj(ptr):
        check_pid(ptr, pid)
    if entry['original_term'] != data:
        print "[pid={0}] copy_shared_perform mismatch:\n  original before = {1}\ncopy after =      {2}".format(pid, entry['original_term'], data)
    if entry['original_term_after'] != data:
        print "[pid={0}] copy_shared_perform mismatch:\n  original before = {1}\ncopy after =      {2}".format(pid, entry['original_term_after'], data)

prefix_list = [
    ( 0, "MINOR GC", "none", None, fun_gc),
    ( 0, "MAJOR GC", "none", None, fun_gc),
    ( 1, "size_object", "ptr", (False, [2]), fun_size_object),
    ( 2, "size was:", "int", None, fun_size_was),
    ( 3, "copy_struct", "ptr", (False, [4]), fun_copy_struct),
    ( 4, "result is at", "ptr", None, fun_result_is_at),
    ( 5, "copy_shared_calculate", "ptr", (True, [6, 2, 7]), fun_copy_shared_calculate),
    ( 6, "message is", "term", None, fun_message_is),
    ( 7, "copy_shared_perform", "ptr", (True, [8, 9, 4]), fun_copy_shared_perform),
    ( 8, "original was", "term", None, fun_original_was),
    ( 9, "copy is", "term", None, fun_copy_is),
    (10, "new message buffer", "none", None, fun_nothing)
    ]

def parse_message(msg):
    for (id, prf, info, next, fun) in prefix_list:
        if msg.startswith(prf):
            n = len(prf) + 1
            if info == "int":
                d = int(msg[n:])
            elif info == "ptr":
                d = int(msg[n+2:], 16)
            elif info == "term":
                d = msg[n:]
            else:
                d = None
            return (id, d, next, fun)
    print "I don't know what this is:", msg

try:
    f = open(sys.argv[1], "rt")
except:
    f = sys.stdin

for line in f:
    re_line = re.compile("(.*)\[pid=([^]]+)\] (.*)\n")
    g = re_line.match(line)
    try:
        prf = g.group(1)
        if prf != "":
            print "Ignoring prefix:", prf
        pid = g.group(2)
        (id, data, next, fun) = parse_message(g.group(3))
    except:
        print "Ignoring line:", line.strip()
    else:
        store(pid, id, data, next, fun)
