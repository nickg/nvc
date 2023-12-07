entity directmap4 is
end entity;

architecture test of directmap4 is
    type t_rec is record
        f : integer;
    end record;

    type t_rec_array is array (natural range <>) of t_rec;

    signal x : t_rec_array(1 to 1);
begin

    b: block is
        port ( p : out t_rec_array(1 to 1) );
        port map ( x );
    begin
        p <= (1 => (f => 1));
    end block;

end architecture;
