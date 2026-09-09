if {[catch {package require fileutil}]} {
    puts stderr "Error: OSVVM requires the Tcllib fileutil package."
    puts stderr "       Install Tcllib (for example: apt-get install tcllib) and retry."
    exit 1
}

source "Scripts/StartNVC.tcl"
set ::osvvm::VhdlLibraryDirectory "${::env(NVC_INSTALL_DEST)}"
set ::osvvm::VhdlLibrarySubdirectory "."
set ::osvvm::AnalyzeErrorStopCount 1

foreach std {2008 2019} {
    SetVHDLVersion $std
    build OsvvmLibraries
}
