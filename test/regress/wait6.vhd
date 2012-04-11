entity wait6 is
end entity;

architecture test of wait6 is
    signal x : bit_vector(3 downto 0);
begin

    print: process (x) is
    begin
        report "x changed " & bit'image(x(3)) & " "
            & bit'image(x(2)) & " "
            & bit'image(x(1)) & " "
            & bit'image(x(0));
    end process;
    
    stim: process is
    begin
        x <= X"1";
        wait for 1 ns;
        x <= X"2";
        wait for 1 ns;
        x <= X"f";
        wait;
    end process;

end architecture;
