entity issue153 is
end entity;

architecture test of issue153 is
    signal s, t : bit_vector(7 downto 0);
begin

    g1: for i in s'range generate
        s(i) <= s(i - 1);
    end generate;

    g2: for i in s'range generate
        t(i - 1) <= s(i);
    end generate;

end architecture;
