entity bounds6 is
end entity;

architecture test of bounds6 is
begin

    process is
        variable i : integer;
        variable c : character;
    begin
        i := 32;
        wait for 1 ns;
        c := character'val(i);
        assert c = ' ';
        i := 1517262;
        wait for 1 ns;
        c := character'val(i);
        wait;
    end process;

end architecture;
