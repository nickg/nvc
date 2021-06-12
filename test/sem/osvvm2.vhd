package OsvvmGlobalPkg is
  type OsvvmOptionsType is (OPT_INIT_PARM_DETECT, OPT_USE_DEFAULT, DISABLED, FALSE, ENABLED, TRUE) ;

  -- Defaults for String values
  constant OSVVM_DEFAULT_ALERT_PREFIX     : string := "%% Alert" ;
  constant OSVVM_DEFAULT_LOG_PREFIX       : string := "%% Log  " ;
  constant OSVVM_DEFAULT_WRITE_PREFIX     : string := "%% " ;
  constant OSVVM_DEFAULT_DONE_NAME        : string := "DONE" ;
  constant OSVVM_DEFAULT_PASS_NAME        : string := "PASSED" ;
  constant OSVVM_DEFAULT_FAIL_NAME        : string := "FAILED" ;
  constant OSVVM_STRING_INIT_PARM_DETECT  : string := NUL & NUL & NUL ;
  constant OSVVM_STRING_USE_DEFAULT       : string := NUL & "" ;

  -- Coverage Settings
  constant OSVVM_DEFAULT_WRITE_PASS_FAIL   : OsvvmOptionsType := FALSE ;
  constant OSVVM_DEFAULT_WRITE_BIN_INFO    : OsvvmOptionsType := TRUE ;
  constant OSVVM_DEFAULT_WRITE_COUNT       : OsvvmOptionsType := TRUE ;
  constant OSVVM_DEFAULT_WRITE_ANY_ILLEGAL : OsvvmOptionsType := FALSE ;

end OsvvmGlobalPkg ;

use work.OsvvmGlobalPkg.all ;

package CoveragePkg is

  -- type OsvvmOptionsType is (OPT_DEFAULT, FALSE, TRUE) ;
  alias CovOptionsType is work.OsvvmGlobalPkg.OsvvmOptionsType ;
  constant COV_OPT_INIT_PARM_DETECT : CovOptionsType := work.OsvvmGlobalPkg.OPT_INIT_PARM_DETECT ;
  -- For backward compatibility.  Don't add to other packages.
  alias DISABLED is work.OsvvmGlobalPkg.DISABLED [return work.OsvvmGlobalPkg.OsvvmOptionsType ];
  alias ENABLED is work.OsvvmGlobalPkg.ENABLED [return work.OsvvmGlobalPkg.OsvvmOptionsType ];

end package CoveragePkg ;
