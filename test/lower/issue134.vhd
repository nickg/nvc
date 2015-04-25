entity issue134 is
end entity;

architecture test of issue134 is
  function bug_function return string is
  begin
    return "";
    return "";                          -- Used to crash here
  end function;
begin
end architecture;
