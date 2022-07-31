entity fifo is
    generic ( n : positive );
    port (
        wdata : bit_vector(1 to n) );
end entity;

architecture test of fifo is
begin
end architecture;

-------------------------------------------------------------------------------

entity neorv1 is
end entity;

architecture test of neorv1 is
    signal x : bit;
    signal y : bit_vector(1 to 2);
begin

    g: for i in 0 to 1 generate
        u: entity work.fifo
            generic map ( 3 )
            port map (
                wdata(1) => x,
                wdata(2 to 3) => y );
    end generate;

end architecture;
