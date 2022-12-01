source "Scripts/StartNVC.tcl"
set ::osvvm::VhdlLibraryDirectory "${::env(NVC_INSTALL_DEST)}"
set ::osvvm::VhdlLibrarySubdirectory "."
source "OsvvmLibraries.pro"
