entity const4 is
end entity;

architecture test of const4 is

    type int2d is array (natural range <>, natural range <>) of integer;

    constant c : int2d := (
        ( 0, 3, 4, 5 ),
        ( 6, 7, 8, 9 ) );

begin

    process is
    begin
        assert c'length(1) = 2;
        assert c'length(2) = 4;
        assert c(0, 0) = 0;
        assert c(0, 1) = 3;
        assert c(0, 3) = 5;
        assert c(1, 0) = 6;
        assert c(1, 1) = 7;
        assert c(1, 2) = 8;
        wait;
    end process;

end architecture;
