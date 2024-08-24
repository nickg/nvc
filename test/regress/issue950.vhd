entity issue950 is
end entity;

architecture test of issue950 is
    type t_base is record
        f : integer_vector;
    end record;

    subtype t_rec is t_base;

    signal s : t_rec(f(1 to 3)) := (f => (others => -1));
begin

    b: block is
        port ( p : inout t_rec );
        port map ( s );
    begin
        p <= (f => (1, 2, 3));
    end block;

    check: process is
    begin
        wait for 1 ns;
        assert s.f = (1, 2, 3);
        wait;
    end process;

end architecture;
