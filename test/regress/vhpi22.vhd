library ieee;
use ieee.std_logic_1164.all;

package vhpi22_pkg is
    type item_array_t is array (natural range <>) of std_logic_vector;
    type width_array_t is array (natural range <>) of positive;
    type matrix_t is array (natural range <>, natural range <>) of std_logic;

    function amax(a : width_array_t) return positive;
end package;

package body vhpi22_pkg is
    function amax(a : width_array_t) return positive is
        variable result : positive := 1;
    begin
        for i in a'range loop
            if a(i) > result then
                result := a(i);
            end if;
        end loop;
        return result;
    end function;
end package body;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.vhpi22_pkg.all;

entity vhpi22_sub is
    generic (
        ITEM_WIDTH : positive;
        WIDTHS     : width_array_t );
    port (
        -- Index constraint is a function call over an array generic so it
        -- cannot be folded during analysis
        p_func   : in item_array_t(amax(WIDTHS) - 1 downto 0)(ITEM_WIDTH - 1 downto 0);
        -- Likewise for an attribute of, or an index into, an array generic
        p_len    : in item_array_t(WIDTHS'length - 1 downto 0)(ITEM_WIDTH - 1 downto 0);
        p_index  : in item_array_t(WIDTHS(0) - 1 downto 0)(ITEM_WIDTH - 1 downto 0);
        -- These have statically foldable bounds and always worked
        p_scalar : in item_array_t(ITEM_WIDTH - 1 downto 0)(ITEM_WIDTH - 1 downto 0);
        p_lit    : in item_array_t(3 downto 0)(ITEM_WIDTH - 1 downto 0);
        -- Every dimension of a multidimensional constraint must be reported
        p_2d     : in matrix_t(amax(WIDTHS) - 1 downto 0, WIDTHS'length - 1 downto 0) );
end entity;

architecture test of vhpi22_sub is
begin
end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.vhpi22_pkg.all;

entity vhpi22 is
end entity;

architecture test of vhpi22 is
    constant WIDTHS_C : width_array_t(0 to 3) := (1, 2, 3, 5);

    signal p_func   : item_array_t(amax(WIDTHS_C) - 1 downto 0)(3 downto 0);
    signal p_len    : item_array_t(WIDTHS_C'length - 1 downto 0)(3 downto 0);
    signal p_index  : item_array_t(WIDTHS_C(0) - 1 downto 0)(3 downto 0);
    signal p_scalar : item_array_t(3 downto 0)(3 downto 0);
    signal p_lit    : item_array_t(3 downto 0)(3 downto 0);
    signal p_2d     : matrix_t(amax(WIDTHS_C) - 1 downto 0, WIDTHS_C'length - 1 downto 0);
begin

    u: entity work.vhpi22_sub
        generic map ( ITEM_WIDTH => 4, WIDTHS => WIDTHS_C )
        port map ( p_func, p_len, p_index, p_scalar, p_lit, p_2d );

end architecture;
