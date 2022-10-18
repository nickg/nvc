variable _dest ${::env(NVC_INSTALL_DEST)}
variable _lib ""

proc DirectoryExists {Directory} {
    return [file exists $Directory]
}

proc include {PathAndFile} {
    set calling_dir [pwd]
    cd [file dirname $PathAndFile]
    puts "Working in [pwd]:"
    source [file tail $PathAndFile]
    cd $calling_dir
}

proc library {LibName} {
    variable _lib
    set _lib $LibName
}

proc analyze {PathAndFile} {
    variable _dest
    variable _lib
    puts "nvc --std=2008 --work=${_lib}:${_dest}/${_lib} -a ${PathAndFile}"
    if {[catch {exec nvc --std=2008 --work=${_lib}:$_dest/$_lib -a $PathAndFile >@ stdout 2>@ stdout} result]} {
        puts stderr "ERROR\n$result"
        exit 1
    }
}

namespace eval osvvm {
    variable ToolVendor NVC
    variable ToolSupportsGenericPackages "true"
}

source "OsvvmLibraries.pro"
