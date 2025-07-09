entity signed1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of signed1 is
    component sub1 is
        generic ( X : integer );
        port ( o : out std_logic );
    end component;

    component sub2 is
        generic ( X : integer );
        port ( o : out std_logic );
    end component;

    signal o1, o2, o3, o4, o5 : std_logic;
begin

    u1: component sub1
        generic map ( -5 )
        port map ( o1 );

    u2: component sub1
        generic map ( 66 )
        port map ( o2 );

    u3: component sub1
        generic map ( 0 )
        port map ( o3 );

    u4: component sub2
        generic map ( -5 )
        port map ( o4 );

    u5: component sub2
        generic map ( 0 )
        port map ( o5 );

    process is
    begin
        wait for 1 ns;
        assert o1 = '0';
        assert o2 = '1';
        assert o3 = 'U';
        assert o4 = '0';
        assert o5 = '1';
        wait;
    end process;

end architecture;
