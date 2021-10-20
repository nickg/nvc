#!/usr/bin/env python3
#
# Usage:
#   opt -time-trace ...
#   llc -time-trace ...
#   llvm-trace.py out.time-trace
#

import json
import sys

with open(sys.argv[1]) as f:
    trace = json.loads(f.read())

namecol = 0
funcs = []
for e in trace['traceEvents']:
    if 'args' in e and 'detail' in e['args']:
        f = e['args']['detail']
        if f == '<stdin>': continue
        name = e['name'].replace('<llvm::Function>', '');
        funcs += [(f, name, e['dur'])]
        namecol = max(namecol, len(name))

for f in sorted(funcs, key=lambda tup: tup[2]):
    print("{:8} : {:{}} : {}".format(f[2], f[1], namecol, f[0]))
