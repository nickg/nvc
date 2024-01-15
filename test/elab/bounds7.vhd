entity bounds7 is
end entity;

architecture test of bounds7 is
    type t_rec is record
        x, y : integer;
    end record;

    type t_rec_array is array (natural range <>) of t_rec;

    function get_value return natural is
    begin
        return 3;
    end function;

    signal s : t_rec_array(1 to get_value);
begin

    b: block is
        generic ( A : natural );
        generic map ( A => 8 );
        port ( t : t_rec_array(1 to A) );
        port map ( s );                 -- Error
    begin
        assert t(A - 1) = (1, 2);
    end block;

end architecture;
