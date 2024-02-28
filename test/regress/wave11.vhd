entity wave11 is
end entity;

architecture test of wave11 is
    signal s : bit_vector(1 to 3);
begin

    b: block is
        port ( p1 : bit_vector );
        port map ( s );

        type t_rec1 is record
            f1 : boolean;
            f2 : p1'subtype;
        end record;

        type t_rec1_array is array (natural range <>) of t_rec1;

        type t_rec2 is record
            f1 : integer;
            f2 : p1'subtype;
            f3 : t_rec1;
            f4 : t_rec1_array(0 to 2);  -- Not dumped currently
        end record;

        signal r : t_rec2;
    begin

        r.f2(1) <= '1' after 1 ns, '0' after 2 ns;
        r.f3.f2(2) <= '1' after 3 ns;

    end block;

end architecture;
