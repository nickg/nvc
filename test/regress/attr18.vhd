entity attr18 is
end entity;

architecture test of attr18 is
begin

    process is
        type t_abc is (a, b, c);
        type t_small is range 1 to 10;
        variable x : integer := 42;
        variable y : t_abc;
    begin
        assert x'image = "42";
        assert x'leftof(5) = 4;
        assert t_abc'length = 3;
        assert y'length = 3;
        assert t_small'length = 10;
        wait;
    end process;

end architecture;
