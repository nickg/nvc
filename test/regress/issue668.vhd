library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

entity sub is
    generic ( WAYS : integer );
end entity;

architecture test of sub is
    type setting_t is record
        ways_bits : integer;
    end record;

    impure function init return setting_t is
    begin
        return (ways_bits => 5);
    end function;

    constant setting : setting_t := init;
begin

    g: for channel in 0 to WAYS-1 generate
        function popcount_channel return integer is
            constant  vec  :  std_logic_vector(SETTING.WAYS_BITS-1 downto 0)
                := std_logic_vector(to_unsigned(channel, SETTING.WAYS_BITS));
            variable  num  :  integer;
            variable  idx  :  integer;
        begin
            num  := 0;
            for i in vec'range loop
                if (vec(i) = '1') then
                    num := num + 1;
                end if;
            end loop;
            return num;
        end function;

        constant k : integer := popcount_channel;
    begin

        p1: process is
        begin
            wait for channel * ns;
            report integer'image(channel) & " ==> " & integer'image(k);
            wait;
        end process;

    end generate;

end architecture;

-------------------------------------------------------------------------------

entity issue668 is
end entity;

architecture test of issue668 is
begin

    u: entity work.sub
        generic map ( 3 );

end architecture;
