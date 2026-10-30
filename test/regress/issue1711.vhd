entity issue1711 is
    generic ( W : natural := 0 );
end entity;

architecture test of issue1711 is
begin
    process
        variable a, b, c : bit_vector(W - 1 downto 0);
        variable d, e, f : bit_vector(-1 downto 0);
        variable s : string(1 to 0);
        constant k : string := s & s;
        constant m : string := "ab" & s;
        constant n : string := s & "ab";
    begin
        c := a & b;
        assert c'length = 0;
        f := d & e;
        assert f'length = 0;
        assert k'length = 0;
        assert m = "ab";
        assert n = "ab";
        wait;
    end process;
end architecture;
