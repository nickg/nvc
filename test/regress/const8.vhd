entity const8 is
end entity;

architecture test of const8 is
begin

    p1: process is
        variable s : string(1 to 12);
    begin
        s := (1 to 11 => "some string", others => NUL);
        wait for 1 ns;
        assert s = "some string" & NUL;

        s := ("foo", "bar", "baz", others => NUL);
        wait for 1 ns;
        report s;
        assert s = "foobarbaz" & NUL & NUL & NUL;

        wait;
    end process;

end architecture;
