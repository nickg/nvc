entity e is
    port ( input : in bit_vector(7 downto 0);
           output : out bit_vector(7 downto 0) );
end entity;

architecture a of e is
begin

    x: entity work.e
        port map (
            input(7 downto 0 => blah,
            output(7 downto 0) => q );

    call(x, y, z);   -- Crash here after earlier error

end architecture;
