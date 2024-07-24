entity record39 is
end entity;

architecture test of record39 is
    type t_rec is record
        x, y : integer;
    end record;

    type t_rec_array is array (natural range <>) of t_rec;

    signal s : t_rec_array(1 to 2);
begin

    b: block is
        port ( p : out t_rec_array );
        port map ( s );
    begin
        p <= ((1, 2), (3, 4));
    end block;

    check: process is
    begin
        wait for 1 ns;
        assert s = ((1, 2), (3, 4));
        wait;
    end process;

end architecture;
