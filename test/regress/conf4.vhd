package pack is
    type t_nat_vec is array (natural range <>) of natural;

    function resolved (arg : t_nat_vec) return natural;

    subtype t_summer is resolved natural;
end package;

package body pack is
    function resolved (arg : t_nat_vec) return natural is
        variable sum : natural := 0;
    begin
        for i in arg'range loop
            sum := sum + arg(i);
        end loop;
        return sum;
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic ( g : natural );
    port ( p : out natural );
end entity;

architecture test of sub is
begin
    p <= g;
end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity top is
end entity;

architecture arch of top is
    component comp is
        port ( x : out t_summer );
    end component;

    function get_true return boolean is
    begin
        return true;
    end function;

    signal r : t_summer;
begin

    u1: component comp port map ( r );

    g1: for i in 1 to 3 generate
        u2: component comp port map ( r );
    end generate;

    b1: block is
    begin
        u3: component comp port map ( r );
    end block;

    g2: if get_true generate
        u4: component comp port map ( r );
    end generate;

    check: process is
    begin
        wait for 1 ns;
        assert r = 117;
        wait;
    end process;

end architecture;

use work.pack.all;

configuration conf4 of top is
    for arch
        for u1 : comp
            use entity work.sub(test)
                generic map ( g => 1 )
                port map ( p => x );
        end for;
        for g1(1 to 3)                  -- XXX: bug here with (1 to 2)
            for u2 : comp
                use entity work.sub(test)
                generic map ( g => 2 )
                port map ( p => x );
            end for;
        end for;
        for b1
            for u3 : comp
                use entity work.sub(test)
                generic map ( g => 10 )
                port map ( p => x );
            end for;
        end for;
        for g2
            for u4 : comp
                use entity work.sub(test)
                generic map ( g => 100 )
                port map ( p => x );
            end for;
        end for;
    end for;
end configuration;
