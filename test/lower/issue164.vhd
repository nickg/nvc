package issue164 is
end package;

package body issue164 is
  procedure same_name(variable var : out integer) is
  begin
  end;

  impure function same_name return integer is
      variable var : integer;
  begin
      return var;
  end function;
end package body;
