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

allrefs = []

def get_ref(prefix, d):
    suffix = os.path.basename(d).upper().replace("-", "_").replace("+", ".")
    return f"{prefix}_{suffix}"


def walk_dir(prefix, d):
    if not os.path.isdir(d):
        raise Exception(f"Non-existent directory {d}")

    fs = glob(f"{d}\\*")
    fs.sort()

    for f in fs:
        ref = get_ref(prefix, f)
        base = os.path.basename(f)
        if os.path.isdir(f):
            print(f"<Directory Id=\"{ref}\" Name=\"{base}\">")
            walk_dir(ref, f)
            print("</Directory>")
        else:
            print(f"<Component Id=\"{ref}\" Win64=\"yes\" DiskId=\"1\"")
            print(f"           Guid=\"{uuid.uuid3(uuid.NAMESPACE_OID, ref)}\">")
            print(f"  <File Id=\"{ref}\" Name=\"{base}\"")
            print(f"        Source=\"{f}\" />")
            print("</Component>")

            allrefs.append(ref)


################################################################################
# DLLs

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

for d in dlls:
    base = os.path.basename(d)
    ref = get_ref("BIN", d)
    print(f"<Component Id=\"{ref}\" Win64=\"yes\" DiskId=\"1\"")
    print(f"           Guid=\"{uuid.uuid3(uuid.NAMESPACE_OID, base)}\">")
    print(f"  <File Id=\"{ref}\" Name=\"{base}\"")
    print(f"        Source=\"{d}\" />")
    print("</Component>")
    allrefs.append(ref)

print("</DirectoryRef>")


################################################################################
# TCL core libraries

print("<DirectoryRef Id=\"LIB\">")

print("<Directory Id=\"TCL\" Name=\"tcl8.6\">")
walk_dir("TCL", f"{wprefix}\\lib\\tcl8.6")
print("</Directory>")

print("<Directory Id=\"TCL8\" Name=\"tcl8\">")
walk_dir("TCL8", f"{wprefix}\\lib\\tcl8")
print("</Directory>")

print("</DirectoryRef>")


################################################################################
# TclLib

print("<DirectoryRef Id=\"LIB\">")

print(f"<Directory Id=\"TCLLIB\" Name=\"tcllib1.21\">")
walk_dir("TCLLIB", f"{sys.argv[2]}\\lib\\tcllib1.21")
print("</Directory>")

print("</DirectoryRef>")


################################################################################
# Compiled VHDL libraries

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


################################################################################
# Emit component groups

print("<ComponentGroup Id=\"LIBS\">")

for r in allrefs:
    print(f"<ComponentRef Id=\"{r}\" />")

print("</ComponentGroup>")
print("</Fragment>")
print("</Wix>")
