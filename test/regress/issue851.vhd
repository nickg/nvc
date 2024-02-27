entity issue851 is
end entity;

architecture test of issue851 is
    signal s : bit_vector(1 to 3);
begin

    b: block is
        port ( p : in bit_vector );
        port map ( s );

        signal t : p'subtype;
    begin
        t <= s after 1 ns;
    end block;

    s <= "101", "111" after 1 ns, "001" after 2 ns;

end architecture;
