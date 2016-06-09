entity jcore6 is
end entity;

architecture test of jcore6 is

    procedure update(signal x : in bit_vector(1 downto 0);
                     sel : in integer;
                     signal y : out bit) is
    begin
        y <= x(sel);
    end procedure;

    signal s_sel : integer range 0 to 1;
    signal s_x : bit_vector(1 downto 0);
    signal s_y : bit;
begin

    update(s_x, s_sel, s_y);

    process is
    begin
        s_x <= "10";
        s_sel <= 1;
        wait for 1 ns;
        assert s_y = '1';
        s_sel <= 0;
        wait for 1 ns;
        assert s_y = '0';
        wait;
    end process;

end architecture;
