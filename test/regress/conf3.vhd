package pack is
    type my_enum is (A, B, C);
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic ( x, y : my_enum );
end entity;

architecture test of sub is
begin

    testp: process is
    begin
        assert x = B;
        assert y = C;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture arch of top is
    component comp is
    end component;
begin

    uut: component comp;

end architecture;

use work.pack.all;

configuration conf3 of top is
    for arch
        for all : comp
            use entity work.sub(test)
                generic map (
                    x => B, y => C );
        end for;
    end for;
end configuration;
