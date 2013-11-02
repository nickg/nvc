entity proc10 is
end entity;

architecture test of proc10 is
begin

    process is
        variable x : integer;
        variable v : bit_vector(7 downto 0);

        procedure change(b : bit) is
        begin
            v(x) := b;
        end procedure;

        procedure change_after(b : bit; t : time) is
        begin
            wait_here: wait for 1 ns;
            change: v(x) := b;
        end procedure;

    begin
        v := X"00";
        x := 1;
        change('1');
        assert v = X"02";
        x := 7;
        change_after('1', 10 ns);
        assert v = X"82";
        wait;
    end process;

end architecture;
