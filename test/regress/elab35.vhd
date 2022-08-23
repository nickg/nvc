library ieee;
use ieee.std_logic_1164.all;

package types_pkg is
    type sl2d_t is array(natural range <>,natural range <>) of std_logic;
    type slv_7_0_t is array(natural range <>) of std_logic_vector(7 downto 0);
    subtype ram_bank_t is slv_7_0_t(0 to 32767);
    type ram_t is array(0 to 3) of ram_bank_t;
end package;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.types_pkg.all;

entity sub is
    generic (
        width      : integer;
        depth_log2 : integer;
        init       : sl2d_t := (0 downto 1 => (0 downto 1 => '0'))
        );
end entity;

architecture test of sub is
    subtype ram_word_t is std_logic_vector(width-1 downto 0);
    type ram_t is array(natural range <>) of ram_word_t;

    function ram_init return ram_t is
        variable r : ram_t(0 to (2**depth_log2)-1);
    begin
        r := (others => (others => '0'));
        if init'high = r'high then
            for i in 0 to r'length-1 loop
                for j in 0 to width-1 loop
                    r(i)(j) := init(i,j);
                end loop;
            end loop;
        end if;
        return r;
    end function ram_init;
    signal ram : ram_t(0 to (2**depth_log2)-1) := ram_init;
begin

end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.types_pkg.all;

entity elab35 is
end entity;

architecture test of elab35 is
    constant size_log2 : integer := 16;

    function rambank2sl2d(constant x : ram_bank_t) return sl2d_t is
        variable r : sl2d_t(0 to (2**(size_log2-2))-1,7 downto 0);
    begin
        for i in 0 to r'length-1 loop
            for j in 0 to 7 loop
                r(i,j) := x(i)(j);
            end loop;
        end loop;
        return r;
    end function rambank2sl2d;

    constant ram_init : ram_t := (others => (others => (others => '0') ) );

begin

    g: for i in 0 to 3 generate
        RAM: entity work.sub
            generic map (
                width      => 8,
                depth_log2 => size_log2-2,
                init       => rambank2sl2d(ram_init(i))
                );
    end generate;
end architecture;
