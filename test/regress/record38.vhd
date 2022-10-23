package my_package is
    type my_type_t is record
        state : bit_vector;
        aux : bit_vector;
    end record my_type_t;

    type my_array_t is array (natural range <>) of my_type_t;
end package my_package;

-------------------------------------------------------------------------------

entity record38 is
end entity;

use work.my_package.all;

architecture test of record38 is
    function init return my_array_t is
    begin
        return (0 => (state => "101", aux => "1"), 1 => (state => "1", aux => "1"));
    end function;

    signal s : my_array_t(0 to 0)(state(1 to 3), aux(1 to 1)) := init;
begin

end architecture;
