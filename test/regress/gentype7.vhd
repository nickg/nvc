entity gentype7 is
end entity;

architecture test of gentype7 is
    signal i : integer;
    signal a : bit_vector(1 to 3);
    signal v : integer_vector(1 to 5);
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

    b3: block is
        package gp is
            generic (
                type element_type is private;
                type index_type is (<>);
                type array_type is array (index_type range <>) of element_type;
                left_index, right_index : index_type );
            signal s : array_type(left_index to right_index);
        end package;

        package p is new gp
            generic map (
                element_type => integer,
                index_type => natural,
                array_type => integer_vector,
                left_index => 1,
                right_index => 5 );
    begin
        p.s <= (1, 2, 3, 4, 5);
        v <= p.s;
    end block;

    check: process is
    begin
        wait for 1 ns;
        assert i = 42;
        assert a = "101";
        assert v = (1, 2, 3, 4, 5);
        wait;
    end process;

end architecture;
