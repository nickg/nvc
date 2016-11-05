package access_field_through_function_pkg is
  type record_t is record
    field : integer;
  end record;

  type protected_t is protected
    function fun return record_t;
  end protected;

  function fun return record_t;
  function fun(param : integer) return record_t;
  function access_field_fun1 return integer;
  function access_field_fun2 return integer;
  function access_field_fun3 return integer;
end package;

package body access_field_through_function_pkg is
  type protected_t is protected body
    function fun return record_t is
    begin
      return (field => 0);
    end function;
  end protected body;

  function fun return record_t is
  begin
    return (field => 0);
  end function;

  function fun(param : integer) return record_t is
  begin
    return (field => param);
  end function;

  function access_field_fun1 return integer is
  begin
    return fun.field; -- <-- does not work
  end function;

  function access_field_fun2 return integer is
      variable x : integer := 10;
  begin
    return fun(x).field; -- <-- works
  end function;

  function access_field_fun3 return integer is
    variable prot : protected_t;
  begin
    return prot.fun.field; -- <-- does not work
  end function;
end package body;

-------------------------------------------------------------------------------

entity issue143 is
end entity;

use work.access_field_through_function_pkg.all;

architecture test of issue143 is
begin

    process is
        variable x : integer := 4;
    begin
        assert fun.field = 0;
        assert fun(x).field = 4;
        assert access_field_fun1 = 0;
        assert access_field_fun2 = 10;
        assert access_field_fun3 = 0;
        wait;
    end process;

end architecture;
