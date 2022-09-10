use std.env.all;

entity wave7 is
end entity;

architecture test of wave7 is
    signal x : integer;
begin

    x <= 1 after 1 ns, 2 after 2 ns, 3 after 4 ns;

    process is
    begin
        wait for 3 ns;
        stop;
    end process;

end architecture;
