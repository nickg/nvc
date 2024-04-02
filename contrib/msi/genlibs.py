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

dlls = []
ldd = subprocess.Popen(["ldd", sys.argv[1]], stdout=subprocess.PIPE)
for line in io.TextIOWrapper(ldd.stdout, encoding="utf-8"):
    parts = line.split()
    dll = parts[0]
    path = parts[2]

    if path.startswith(prefix):
        dlls.append(path.replace(prefix, wprefix))

dlls.sort()


def get_ref(d):
    return os.path.basename(d).upper().replace("-", "_").replace("+", "_")


for d in dlls:
    base = os.path.basename(d)
    ref = get_ref(d)
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
        ref = get_ref(t)
        print(f"<Component Id=\"{ref}\" Win64=\"yes\" DiskId=\"1\"")
        print(f"           Guid=\"{uuid.uuid3(uuid.NAMESPACE_OID, base)}\">")
        print(f"  <File Id=\"{ref}\" Name=\"{base}\"")
        print(f"        Source=\"{t}\" />")
        print("</Component>")


print("</DirectoryRef>")

print("<ComponentGroup Id=\"LIBS\">")

for d in dlls:
    print(f"<ComponentRef Id=\"{get_ref(d)}\" />")

for t in tcl:
    if os.path.isdir(t):
        pass
    else:
        print(f"<ComponentRef Id=\"{get_ref(t)}\" />")

print("</ComponentGroup>")
print("</Fragment>")
print("</Wix>")
