entity dummy is
    port(
        input  : in bit;
        output : out bit
    );
end entity;

architecture rtl of dummy is
begin
    output <= input;
end architecture;

------------------------------------------------------------------------
-- Top-level example
------------------------------------------------------------------------

entity scope_demo is
end entity;

architecture rtl of scope_demo is

    signal u : bit_vector(3 downto 0) := "0000";

begin

    -- this is the problematic part:
    gen : for i in 0 to 3 generate
    begin
        u : entity work.dummy
            port map(
                input  => u(i),
                output => open
            );
    end generate;

    -- this is handled correctly:
    u : entity work.dummy
        port map(
            input  => u(0),
            output => open
        );

    -- extended checks
    ex : block
        constant c0 : bit := scope_demo(0);
        constant c1 : bit := rtl(0);
        constant c2 : bit := std(0);
        constant c3 : bit := std.standard(0);
        constant c4 : bit_vector(1 downto 0) := scope_demo(1 downto 0);
        constant c5 : bit_vector(1 downto 0) := rtl(1 downto 0);
        constant c7 : bit_vector(1 downto 0) := std(1 downto 0);
        constant c6 : bit_vector(1 downto 0) := std.standard(1 downto 0);
    begin
    end block;

end architecture;
