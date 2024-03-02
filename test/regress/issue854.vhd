entity issue854 is
end entity;

architecture test of issue854 is
    type t_rec1 is record
        f : integer;
    end record;

    type t_rec2 is record
        a : integer;
        b : t_rec1;
    end record;

    view t_rec2_view of t_rec2 is
        a : in;
        b : out;
    end view;

    signal s1, s2 : t_rec2;
begin

    b1: block is
        port ( p : view t_rec2_view);
        port map ( s1 );
    begin
        p.b.f <= p.a + 1;
    end block;

    b2: block is
        port ( p : view t_rec2_view'converse);
        port map ( s2 );
    begin
        p.a <= p.b.f * 2;
    end block;

    check: process is
    begin
        s1.a <= 1;
        s2.b.f <= 5;
        wait for 0 ns;
        wait for 0 ns;
        assert s1.b.f = 2;
        assert s2.a = 10;
        wait;
    end process;

end architecture;
