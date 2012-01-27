library ieee;
use ieee.std_logic_1164.all;

entity ieee1 is
end entity;

architecture test of ieee1 is
begin

    process is
        variable a, b, c : std_logic_vector(3 downto 0);
        variable d : std_logic_vector(5 downto 0);
    begin
        a := ( '0', '1', '0', '0' );
        b := ( '1', '0', '1', '0' );
        assert_a: assert ((a(3) or b(3)) = '1') report "a";
        c := (a or b);
        assert c(3) = '1' report "c(3)";
        assert c(0) = '0' report "c(0)";
        assert_b: assert c = ( '1', '1', '1', '0' ) report "b";
        d(4 downto 1) := (a or b);
        assert d = ( 'U', '1', '1', '1', '0', 'U' );
        wait;
    end process;
    
end architecture;
