entity attr11 is
end entity;

architecture test of attr11 is
begin

    process is
        variable i : integer;
        attribute a : bit_vector;
        attribute a of i : variable is "101";
        attribute b : integer;
        attribute b of i : variable is 4;
    begin
        assert i'a(1) = '0';
        assert i'a = "101";
        wait;
    end process;

end architecture;
