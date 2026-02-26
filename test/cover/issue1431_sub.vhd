entity issue1431_sub is
    port ( o : out bit );
end entity;

architecture test of issue1431_sub is
    signal x : bit;
begin

    p: process is
    begin
        -- coverage off
        x <= '1';
        wait;
        -- coverage on
    end process;

    transfer: o <= x;                             -- Covered

end architecture;
