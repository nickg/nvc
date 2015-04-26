entity access_bug is
end entity;

architecture test of access_bug is
  type integer_access is access integer;
  type integer_access_access is access integer_access;
  type integer_access_array is array (natural range <>) of integer_access;
  type integer_access_array_access is access integer_access_array;
  procedure bug_procedure is
    variable arr : integer_access_array_access := null;
    variable ia : integer_access_access := null;
  begin
    ia := new integer_access; -- <-- or here
    arr := new integer_access_array(0 to 10); -- <--bug here
  end;
begin
end architecture;
