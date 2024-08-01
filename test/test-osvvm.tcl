# Clones OsvvmLibraries, runs all tests. Used by the test-osvvm.yml workflow.
# To run manually:
#   tclsh test-osvvm.tcl
# Note: tcllib is required for OSVVM

if {[info exists env(OSVVM_DIR)]} {
    set OsvvmDir $env(OSVVM_DIR)
} else {
    set scriptdir [ file dirname [ file normalize [ info script ] ] ]
    set fid [open "$scriptdir/../contrib/install-osvvm.sh" r]
    while {[gets $fid line] != -1} {
        if {[regexp -all -- {git_wrapper \S+ (\S+)} $line -> tag]} {
            break
        }
    };
    close $fid;

    set OsvvmDir [file join $::env(HOME) .cache nvc "OsvvmLibraries-$tag"]
}

source $OsvvmDir/Scripts/StartNVC.tcl

#SetVHDLVersion 2019
SetExtendedElaborateOptions {--jit}

set ::osvvm::AnalyzeErrorStopCount 1
set ::osvvm::SimulateErrorStopCount 1

if {[info exists env(OSVVM_MUST_BUILD)]} {
    build $OsvvmDir/OsvvmLibraries
}

build $OsvvmDir/RunAllTests
build $OsvvmDir/RunAllTestsVti
