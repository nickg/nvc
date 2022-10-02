entity sub is
    port (
        a : in bit_vector );
end entity;

architecture test of sub is
begin

    process (a)
    begin
        report a'path_name & " range is " & integer'image(a'left)
            & " to " & integer'image(a'right) ;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity elab10 is
end entity;

architecture test of elab10 is
    signal x : bit_vector(1 to 5);
    signal y : bit_vector(6 to 10);
begin

    sub1_i: entity work.sub
        port map ( x );

    sub2_i: entity work.sub
        port map ( y );

    process is
    begin
        y <= not y after 1 ns;
        wait;
    end process;

end architecture;
