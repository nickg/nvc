entity vecorder1 is
end entity;

architecture test of vecorder1 is
    type int_array is array (integer range <>) of integer;

    signal s : int_array(0 to 1) := ( 0 => 0, 1 => 1 );
begin

    process is
        variable x : int_array(0 to 1) := ( 0 => 0, 1 => 1 );
        variable y : int_array(1 downto 0) := ( 0 => 0, 1 => 1 );
    begin
        assert x(0) = 0 report "one";
        assert x(1) = 1 report "two";
        assert x = ( 0, 1 );
        x := ( 2, 3 );
        report integer'image(x(0));
        report integer'image(x(1));
        assert x(0) = 2 report "three";
        assert x(1) = 3 report "four";
        assert x = ( 2, 3 ) report "five";
        assert ( 2, 3 ) = x report "six";

        assert s(0) = 0 report "s one";
        assert s(1) = 1 report "s two";
        s <= ( 2, 3 );
        wait for 0 ns;
        report integer'image(s(0));
        report integer'image(s(1));
        assert s(0) = 2 report "s three";
        assert s(1) = 3 report "s four";

        assert y(0) = 0 report "y one";
        assert y(1) = 1 report "y two";
        assert y = ( 1, 0 );
        y := ( 2, 3 );
        report integer'image(y(0));
        report integer'image(y(1));
        assert y(0) = 3 report "y three";
        assert y(1) = 2 report "y four";
        assert y = ( 2, 3 ) report "y five";

        wait;
    end process;

end architecture;
