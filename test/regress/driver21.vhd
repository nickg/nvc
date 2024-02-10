entity driver21 is
end entity;

architecture test of driver21 is
    type t_rec is record
        x, y : natural;
    end record;

    type t_rec_array is array (natural range <>) of t_rec;

    function resolved (r : t_rec_array) return t_rec is
    begin
        assert r(0).x = 1 severity failure;
        assert r(0).y = 2 severity failure;
        return r(0);
    end function;

    subtype t_resolved_rec is resolved t_rec;

    signal s : t_resolved_rec := (1, 2);
begin

    s <= (6, 7) after 1 ns;
    s <= (3, 4);

end architecture;
