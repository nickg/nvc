architecture a of e is
begin

    x: entity work.foo
        port map (
            input(7 downto 0 => blah,
            output(7 downto 0) => q );

    call(x, y, z);   -- Crash here after earlier error

end architecture;
