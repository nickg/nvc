package outer is
    package inner is
        generic ( N : integer );
        --impure function get_n return integer;
    end package;
end package;

-- package body outer is
--     package body inner is
--         impure function get_n return integer is
--         begin
--             return n;
--         end function;
--     end package body;
-- end package body;

entity genpack13 is
end entity;

use work.outer.all;

architecture test of genpack13 is
    package inner4 is new inner generic map ( 4 );
    package inner5 is new inner generic map ( 5 );
begin

    p1: process is
    begin
        assert inner4.n = 4;
        assert inner5.n = 5;
        wait;
    end process;

end architecture;
