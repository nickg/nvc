library ieee;
use ieee.std_logic_1164.all;

entity sub is
    port ( data : inout std_logic_vector(15 downto 0) := (others => 'Z') );
end entity;

architecture test of sub is
    signal data_int : std_logic_vector(15 downto 0);
begin

    p1: data_int <= data;

    p2: process is
    begin
        wait for 1 ns;
        assert data_int = X"0000";
        wait for 1 ns;
        assert data_int = X"0047";
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity issue609 is
end entity;

architecture test of issue609 is
    signal data : std_logic_vector(15 downto 0) := X"0000";

    procedure drive_half (signal x : inout std_logic_vector(15 downto 0)) is
    begin
        x(7 downto 0) <= X"47";
        wait for 1 ns;
    end procedure;

begin

    u: entity work.sub port map ( data );

    p3: process is
    begin
        data <= X"0000";
        wait for 1 ns;
        drive_half(data);
        wait;
    end process;

end architecture;
