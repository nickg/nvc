-- Was test/regress/alias6.vhd
entity alias1 is
end entity;

architecture test of alias1 is
    signal x : bit_vector(7 downto 0);
    alias x_top is x(7);
    alias x_low is x(3 downto 0);
    alias x_high is x(7 downto 4);
begin

    process is
    begin
        x <= X"80";
        wait for 1 ns;
        x <= X"04";
        wait for 1 ns;
        x_top <= '1';
        wait for 1 ns;
        x_high <= X"f";
        wait for 1 ns;
        x_low <= X"b";
        x_high <= X"1";
        wait;
    end process;

end architecture;
