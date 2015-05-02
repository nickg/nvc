package pkg is
  type integer_vector is array (natural range <>) of integer;
  type integer_vector_ptr is access integer_vector;
  procedure get(variable vec : in integer_vector_ptr; sum : inout integer);
end package;

package body pkg is
  procedure get(variable vec : in integer_vector_ptr; sum : inout integer) is
  begin
      sum := 0;
      for i in vec.all'range loop
          sum := sum + vec.all(i);
      end loop;
  end procedure;
end package body;

-------------------------------------------------------------------------------

entity issue152 is
end entity;

use work.pkg.all;

architecture test of issue152 is
begin

    process is
        variable sum : integer;
        variable vec : integer_vector_ptr;
    begin
        vec := new integer_vector'(1, 2, 3, 4, 5);
        get(vec, sum);
        assert sum = 15;
        wait;
    end process;

end architecture;
