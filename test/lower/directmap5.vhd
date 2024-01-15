entity directmap5 is
end entity;

architecture test of directmap5 is
    type t_rec is record
        x, y : integer;
    end record;

    type t_rec_array is array (natural range <>) of t_rec;

    function get_value return natural is
    begin
        return 3;
    end function;

    signal s : t_rec_array(1 to 3);
begin

    b: block is
        port ( t : t_rec_array(1 to get_value) );
        port map ( s );
    begin
    end block;

end architecture;
