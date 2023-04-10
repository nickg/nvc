entity d is
    generic (c : positive);
    port (
        b : in bit
        );
end d;

architecture beh of d is
    constant g : time := 1 ms / real(c);
begin
    process
    begin
        wait until b = '1';
        wait for -g;                    -- Error
    end process;
end architecture;

entity e is
end e;

architecture sim of e is
    signal a : bit;
begin
    F : entity work.d(beh)
        generic map(
            c => 1 )
        port map(
            b => a );
end architecture;
