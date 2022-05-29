entity attr16 is
end entity;

architecture test of attr16 is

    type int_vec_2d is array (natural range <>, natural range <>) of integer;

    signal s1, s2 : integer_vector(1 to 5) := (others => 0);
    signal s3, s4 : int_vec_2d(1 to 2, 5 to 5) := (others => (others => 0));
begin

    check: process is
    begin
        assert s1'range(1)'low = 1;
        assert s1'range(1)'right = 5;
        assert s3'range(1)'left = 1;
        assert s3'range(2)'left = 5;
        assert s3'range(2)'right = 5;
        wait;
    end process;

end architecture;
