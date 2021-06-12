package AlertLogPkg is

  subtype  AlertLogIDType       is integer ;
  type     AlertLogIDVectorType is array (integer range <>) of AlertLogIDType ;
  type     AlertType        is (FAILURE, ERROR, WARNING) ;  -- NEVER
  subtype  AlertIndexType   is AlertType range FAILURE to WARNING ;
  type     AlertCountType   is array (AlertIndexType) of integer ;

end AlertLogPkg ;

package body AlertLogPkg is

  type AlertLogStructPType is protected

    impure function GetDisabledAlertCount return AlertCountType ;
    impure function GetDisabledAlertCount(AlertLogID: AlertLogIDType) return AlertCountType ;

  end  protected AlertLogStructPType ;

  type AlertLogStructPType is protected body

    impure function GetDisabledAlertCount return AlertCountType is
      variable Count : AlertCountType := (others => 0) ;
    begin
      return Count ;
    end function GetDisabledAlertCount ;

  end protected body AlertLogStructPType ;

  shared variable AlertLogStruct : AlertLogStructPType ;

  impure function GetDisabledAlertCount return AlertCountType is
    variable result : AlertCountType ;
  begin
    result := AlertLogStruct.GetDisabledAlertCount ;  -- OK
    return result ;
  end function GetDisabledAlertCount ;

end package body AlertLogPkg ;
