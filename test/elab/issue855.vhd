entity B is
    port (
        clk : in bit;
        f : in bit
        );
end B;

architecture rtl of B is
begin
end architecture;

entity C is
port (
    clk : in bit;
    e : in bit
    );
end C;

architecture rtl of C is
signal f : bit;
begin
end architecture;

entity issue855 is
end issue855;

architecture sim of issue855 is
    signal clk : bit;
    signal e : bit;
begin
    G : entity work.C(rtl)
        port map (
            clk => clk,
            e => e
            );

    H : entity work.B(rtl)
        port map (
            clk => clk,
            f => << signal G.f : bit >>
            );
end architecture;
