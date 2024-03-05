entity driver22 is
end entity;

architecture test of driver22 is
    type t_rec is record
        f, g : integer;
    end record;

    type t_rec_array is array (natural range <>) of t_rec;

    view t_rec_in of t_rec is
        f : in;
        g : out;
    end view;

    signal s : t_rec;
begin
    b: block is
        port ( p : view (t_rec_in) of t_rec_array );
        port map ( p(0) => s );
    begin
        process (all) is
            variable sel : natural := 0;
        begin
            p(sel).g <= p(sel).f + 1;
        end process;
    end block;
end architecture;
