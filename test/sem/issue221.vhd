package fred4 is

  type fred4_t is protected
    impure function is_empty return boolean;
    impure function hi_there return string;
  end protected fred4_t;

end package fred4;

package body fred4 is

  type fred4_t is protected body
    ----------------------------------------
    impure function is_empty return boolean is
    begin
      return TRUE;
    end function is_empty;
    ----------------------------------------
    impure function hi_there return string is
    begin
      if is_empty then
        return "perfect";
      else
        return "we have a problem";
      end if;
    end function hi_there;
    ----------------------------------------
  end protected body fred4_t;

end package body fred4;
