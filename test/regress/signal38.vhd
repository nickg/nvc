entity signal38 is
end entity;

architecture test of signal38 is
    type t_byte is range 255 downto 0;
    type t_word is range 65535 downto 0;
    type t_word_array is array (natural range <>) of t_word;
    type t_int_array is array (natural range <>) of integer;

    constant c1 : t_int_array(1 to 3) := (1, 2, 3);
    constant c2 : t_word_array(1 to 3) := t_word_array(c1);  -- Folded

    signal s1 : t_byte := 5;
    signal s2 : t_word_array(1 to 3) := (1, 2, 3);
begin

    check: process is
    begin
        assert s1 = 5;
        assert s2 = (1, 2, 3);
        assert c2 = (1, 2, 3);
        s1 <= 200;
        s2(2) <= 1000;
        wait for 1 ns;
        assert s1 = 200;
        assert s2 = (1, 1000, 3);
        wait;
    end process;

end architecture;
