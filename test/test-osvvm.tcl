# Clones OsvvmLibraries, runs all tests. Used by the test-osvvm.yml workflow.
# To run manually:
#   tclsh test-osvvm.tcl
# Note: tcllib is required for OSVVM

# known good snapshot
set OsvvmLibraries_tag "2022.11"

exec git clone --branch $OsvvmLibraries_tag --recurse-submodules -j8 https://github.com/OSVVM/OsvvmLibraries.git >@ stdout 2>@ stdout
source OsvvmLibraries/Scripts/StartNVC.tcl
build OsvvmLibraries/OsvvmLibraries
build OsvvmLibraries/RunAllTests
build OsvvmLibraries/RunAllTestsVti
