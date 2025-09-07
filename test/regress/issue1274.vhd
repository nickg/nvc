entity issue1274 is
end entity issue1274;

architecture beh of issue1274 is
    type arr_t is array (natural range<>) of integer;
    type arr2_t is array (natural range<>) of arr_t; -- element is unconstrained (allowed in 2019)
    constant carr2 : arr2_t(0 to 99)(0 to 3) := (others => (others => 0));
    -- workaround (actually constrain the inner element)
    --type arr2_t is array (natural range<>) of arr_t(0 to 3); -- element is actually constrained
    --constant carr2 : arr2_t(0 to 99) := (others => (others => 0));
    signal arr12 : arr2_t(0 to 24) := carr2(25 to 49);
    signal val : integer;

    type t_rec is record
        f, g : bit_vector;
    end record;

    constant crec : t_rec(f(10 to 13), g(0 to 2)) := ("1010", "111");
    signal srec : t_rec(g(20 to 22)) := crec;

    signal arr13 : arr2_t(open)(10 to 13) := carr2(25 to 49);
begin
    val <= arr12(0)(3); -- Error: index 0 is out of range 25 to 49

    process is
    begin
        assert arr12'left = 0;
        assert arr12'right = 24;
        assert arr12'element'left = 0;
        assert arr12'element'right = 3;

        assert crec.f'left = 10;
        assert crec.f'right = 13;
        assert crec.g'left = 0;
        assert crec.g'right = 2;

        assert srec.f'left = 10;
        assert srec.f'right = 13;
        assert srec.g'left = 20;
        assert srec.g'right = 22;

        assert arr13'left = 25;
        assert arr13'right = 49;
        assert arr13'element'left = 10;
        assert arr13'element'right = 13;

        wait;
    end process;
end architecture beh;
