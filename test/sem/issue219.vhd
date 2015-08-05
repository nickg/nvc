package fred2 is
  function hi_there return string;
end package fred2;

package body fred2 is

  type line is access string;

  function hi_there return string is
    variable ln : line;
  begin
    if null = ln then                   -- Was error
      return "Proper initial value for access type.";
    else
      return "Compiler error.";
    end if;
  end function hi_there;

end package body fred2;
