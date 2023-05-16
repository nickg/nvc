package pack is
    type counter_t is protected
        procedure increment;
        impure function get return integer;
    end protected;
end package;

package body pack is

    type counter_t is protected body
        variable value : natural;

        procedure increment is
        begin
            value := value + 1;
        end procedure;

        impure function get return integer is
        begin
            return value;
        end function;
    end protected body;

end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( variable sv : inout counter_t );
end entity;

architecture test of sub is
begin
    p1: process is
    begin
        for i in 1 to 10 loop
            sv.increment;
            wait for 1 ns;
        end loop;
        wait;
    end process;
end architecture;

-------------------------------------------------------------------------------

entity protected9 is
end entity;

use work.pack.all;

architecture test of protected9 is
    shared variable sv : counter_t;
begin

    u: entity work.sub port map ( sv );

    check: postponed process is
    begin
        assert sv.get = 1;
        wait for 5 ns;
        assert sv.get = 5;
        wait for 5 ns;
        assert sv.get = 10;
        wait;
    end process;

end architecture;
