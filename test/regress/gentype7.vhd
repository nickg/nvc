entity gentype7 is
end entity;

architecture test of gentype7 is
    signal i : integer;
    signal a : bit_vector(1 to 3);
begin

    b1: block is
        generic ( type t is range <> );
        generic map ( t => integer );

        signal s : t;
    begin
        i <= integer(s);
        s <= 42;
    end block;

    b2: block is
        generic ( type e is (<>);
                  type i is range <>;
                  type t is array (i range <>) of bit );
        generic map ( e => bit, i => natural, t => bit_vector );

        signal s : t(1 to 3);
    begin
        a <= bit_vector(s);
        s <= "101";
    end block;

    check: process is
    begin
        wait for 1 ns;
        assert i = 42;
        assert a = "101";
        wait;
    end process;

end architecture;
