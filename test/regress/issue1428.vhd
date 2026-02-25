entity issue1428 is
end entity;

architecture test of issue1428 is
    type t_rec is record
        x : integer;
        y : bit_vector(3 downto 0);
    end record;

    type t_rec_array is array (natural range <>) of t_rec;

    signal r : t_rec;
    signal s : bit_vector(2 downto 0);
    signal a : t_rec_array(1 to 3);
begin

    process is
    begin
        s <= "101";
        r <= (5, "1000");
        a(1) <= (7, "0000");
        wait for 1 fs;
        s <= "000";
        r <= (5, "0001");
        wait for 1 fs;
        s <= "111";
        a(2) <= (8, "1111");
        wait;
    end process;

end architecture;
