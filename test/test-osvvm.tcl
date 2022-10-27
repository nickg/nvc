# Clones OsvvmLibraries, runs all tests. Used by the test-osvvm.yml workflow.
# To run manually:
#   tclsh test-osvvm.tcl
# Note: tcllib is required for OSVVM

# known good snapshot
set OsvvmLibraries_tag "2022.09"
set OsvvmScripts_tag "e425576fbc481bd6528d3e7bfea3e2bf5800e199"; # added NVC

exec git clone --branch $OsvvmLibraries_tag --recurse-submodules -j8 https://github.com/OSVVM/OsvvmLibraries.git >@ stdout 2>@ stdout
cd OsvvmLibraries/Scripts
exec git checkout $OsvvmScripts_tag >@ stdout 2>@ stdout
cd ../..
source OsvvmLibraries/Scripts/StartNVC.tcl
build OsvvmLibraries/OsvvmLibraries
build OsvvmLibraries/RunAllTests
build OsvvmLibraries/RunAllTestsVti
