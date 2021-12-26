package AlertLogPkg is

--  type   AlertLogIDType       is range integer'low to integer'high ; -- next revision
  subtype  AlertLogIDType       is integer ;
  type     AlertLogIDVectorType is array (integer range <>) of AlertLogIDType ;
  type     AlertType        is (FAILURE, ERROR, WARNING) ;  -- NEVER
  subtype  AlertIndexType   is AlertType range FAILURE to WARNING ;
  type     AlertCountType   is array (AlertIndexType) of integer ;
  type     AlertEnableType  is array(AlertIndexType) of boolean ;
  type     LogType          is (ALWAYS, DEBUG, FINAL, INFO, PASSED) ;  -- NEVER  -- See function IsLogEnableType
  subtype  LogIndexType     is LogType range DEBUG to PASSED ;
  type     LogEnableType    is array (LogIndexType) of boolean ;


  type AlertLogStructPType is protected

    ------------------------------------------------------------
    procedure alert (
    ------------------------------------------------------------
      AlertLogID   : AlertLogIDType ;
      message      : string ;
      level        : AlertType := ERROR
    ) ;


  end  protected AlertLogStructPType ;

end AlertLogPkg ;

--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////

package body AlertLogPkg is

-- synthesis translate_off

  -- instead of justify(to_upper(to_string())), just look up the upper case, left justified values
  type     AlertNameType is array(AlertType) of string(1 to 7) ;
  constant ALERT_NAME : AlertNameType := (WARNING => "WARNING", ERROR => "ERROR  ", FAILURE => "FAILURE") ;  -- , NEVER => "NEVER  "

  --- ///////////////////////////////////////////////////////////////////////////

  type AlertLogStructPType is protected body


    ------------------------------------------------------------
    -- Report formatting settings, with defaults
    variable PrintPassedVar              : boolean := TRUE ;
    variable PrintAffirmationsVar        : boolean := FALSE ;
    variable PrintDisabledAlertsVar      : boolean := FALSE ;
    variable PrintRequirementsVar        : boolean := FALSE ;
    variable HasRequirementsVar          : boolean := FALSE ;
    variable PrintIfHaveRequirementsVar  : boolean := TRUE ;

    variable DefaultPassedGoalVar        : integer := 1 ;

    ------------------------------------------------------------
    procedure alert (
    ------------------------------------------------------------
      AlertLogID   : AlertLogIDType ;
      message      : string ;
      level        : AlertType := ERROR
    ) is
    begin
        report ALERT_NAME(Level);  -- Lower crash here
    end procedure alert ;

  end protected body AlertLogStructPType ;

end package body AlertLogPkg ;

-------------------------------------------------------------------------------

-- "GC removed all objects from arena ???" crash here
entity e is
end entity;
