source "Scripts/StartNVC.tcl"
set ::osvvm::VhdlLibraryDirectory "${::env(NVC_INSTALL_DEST)}"
set ::osvvm::VhdlLibrarySubdirectory "."

foreach std {2008} {
    SetVHDLVersion $std
    source "OsvvmLibraries.pro"
}

