import io
import subprocess
import sys
import os
import uuid
from glob import glob

print("<?xml version=\"1.0\" encoding=\"utf-8\"?>")
print("<Wix xmlns=\"http://schemas.microsoft.com/wix/2006/wi\">")
print("<Fragment Id=\"LibsFragment\">")

print("<DirectoryRef Id=\"BIN\">")

wprefix = os.getenv("MSYSTEM_PREFIX")
prefix = subprocess.check_output(
    ["cygpath", "-u", wprefix]).decode("utf-8").strip()

dlls = set()
ldd = subprocess.Popen(["ldd", sys.argv[1]], stdout=subprocess.PIPE)
for line in io.TextIOWrapper(ldd.stdout, encoding="utf-8"):
    parts = line.split()
    dll = parts[0]
    path = parts[2]

    if path.startswith(prefix):
        dlls.add(path.replace(prefix, wprefix))

dlls = list(dlls)
dlls.sort()


def get_ref(prefix, d):
    suffix = os.path.basename(d).upper().replace("-", "_").replace("+", "_")
    return f"{prefix}_{suffix}"


for d in dlls:
    base = os.path.basename(d)
    ref = get_ref("BIN", d)
    print(f"<Component Id=\"{ref}\" Win64=\"yes\" DiskId=\"1\"")
    print(f"           Guid=\"{uuid.uuid3(uuid.NAMESPACE_OID, base)}\">")
    print(f"  <File Id=\"{ref}\" Name=\"{base}\"")
    print(f"        Source=\"{d}\" />")
    print("</Component>")

print("</DirectoryRef>")

print("<DirectoryRef Id=\"TCL\">")

tcl = glob(wprefix + "\\lib\\tcl8.6\\*")
tcl.sort()

for t in tcl:
    if os.path.isdir(t):
        pass
    else:
        base = os.path.basename(t)
        ref = get_ref("LIB_TCL", t)
        print(f"<Component Id=\"{ref}\" Win64=\"yes\" DiskId=\"1\"")
        print(f"           Guid=\"{uuid.uuid3(uuid.NAMESPACE_OID, base)}\">")
        print(f"  <File Id=\"{ref}\" Name=\"{base}\"")
        print(f"        Source=\"{t}\" />")
        print("</Component>")

print("</DirectoryRef>")


libdirs = [
    ("LIB_NVC_NVC", "\\lib\\nvc\\nvc"),
    ("LIB_NVC_NVC.08", "\\lib\\nvc\\nvc.08"),
    ("LIB_NVC_NVC.19", "\\lib\\nvc\\nvc.19"),
    ("LIB_NVC_STD", "\\lib\\nvc\\std"),
    ("LIB_NVC_STD.08", "\\lib\\nvc\\std.08"),
    ("LIB_NVC_STD.19", "\\lib\\nvc\\std.19"),
    ("LIB_NVC_IEEE", "\\lib\\nvc\\ieee"),
    ("LIB_NVC_IEEE.08", "\\lib\\nvc\\ieee.08"),
    ("LIB_NVC_IEEE.19", "\\lib\\nvc\\ieee.19"),
]
allrefs = []

for (prefix, folder) in libdirs:
    print(f"<DirectoryRef Id=\"{prefix}\">")

    files = glob(sys.argv[2] + folder + "\\*")
    files.sort()

    for f in files:
        if os.path.isdir(f):
            next

        base = os.path.basename(f)
        ref = get_ref(prefix, f)
        guid = uuid.uuid3(uuid.NAMESPACE_OID, folder + "\\" + base)
        print(f"<Component Id=\"{ref}\" Win64=\"yes\" DiskId=\"1\"")
        print(f"           Guid=\"{guid}\">")
        print(f"  <File Id=\"{ref}\" Name=\"{base}\"")
        print(f"        Source=\"{f}\" />")
        print("</Component>")

        allrefs.append(ref)

    print("</DirectoryRef>")


print("<ComponentGroup Id=\"LIBS\">")

for d in dlls:
    print(f"<ComponentRef Id=\"{get_ref('BIN', d)}\" />")

for t in tcl:
    if os.path.isdir(t):
        pass
    else:
        print(f"<ComponentRef Id=\"{get_ref('LIB_TCL', t)}\" />")

for r in allrefs:
    print(f"<ComponentRef Id=\"{r}\" />")

print("</ComponentGroup>")
print("</Fragment>")
print("</Wix>")
