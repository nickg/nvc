entity bounds12 is
end entity;

architecture test of bounds12 is
begin

    process is
    begin
        assert integer'value("hello") = 5;
        wait;
    end process;

end architecture;
