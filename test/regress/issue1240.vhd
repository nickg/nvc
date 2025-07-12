package funcs is
    function get_params return integer_vector;
    function sum (x : integer_vector) return integer;
end package;

package body funcs is
    function get_params return integer_vector is
    begin
        return (1, 2, 3);
    end function;

    function sum (x : integer_vector) return integer is
        variable r : integer := 0;
    begin
        for i in x'range loop
            r := r + x(i);
        end loop;
        return r;
    end function;
end package body;

-------------------------------------------------------------------------------

use work.funcs.all;

package types is
    constant params : integer_vector := get_params;
    constant width : integer := sum(params);
end package;

-------------------------------------------------------------------------------

entity issue1240 is
end entity;

use work.types.all;

architecture test of issue1240 is
    type t1 is array (natural range <>) of bit_vector(width - 1 downto 0);
    type t2 is array (1 to 3) of bit_vector(width - 1 downto 0);
begin
end architecture;
