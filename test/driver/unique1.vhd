entity unique1 is
end entity;

architecture test of unique1 is
    type t_rec is record
        a, b : integer;
        c : bit_vector(1 to 3);
    end record;

    signal s1 : bit_vector(1 downto 0);
    signal s2 : bit_vector(2 downto 0);
    signal s3 : bit_vector(7 downto 3);
    signal s4 : t_rec;
    signal s5 : t_rec;
begin

    s1(1) <= '1';
    s1(0) <= '0';

    s2(1) <= '1';
    s2(0) <= '0';

    s3(7 downto 7) <= "1";
    s3(6 downto 4) <= "101";
    s3(3) <= '0';

    s4.a <= 5;
    s4.b <= 7;
    s4.c <= "101";

    s5.a <= 5;
    s5.b <= 7;
    s5.c(1 to 2) <= "11";
end architecture;
