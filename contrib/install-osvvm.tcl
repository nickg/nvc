source "Scripts/StartNVC.tcl"
set ::osvvm::VhdlLibraryDirectory "${::env(NVC_INSTALL_DEST)}"
set ::osvvm::VhdlLibrarySubdirectory "."
set ::osvvm::AnalyzeErrorStopCount 1

foreach std {2008 2019} {
    SetVHDLVersion $std
    build OsvvmLibraries
}
