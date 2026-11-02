entity issue1637 is
end entity;

architecture test of issue1637 is
    function make (idx : integer; len : positive) return bit_vector is
        variable v : bit_vector(len - 1 downto 0);
    begin
        v := (idx => '1', others => '0');
        return v;
    end function;
begin
    p1: process is
        variable v : bit_vector(1 to 4);
        variable n : integer := 3;
    begin
        assert make(2, 4) = "0100";
        assert make(0, 4) = "0001";
        assert make(3, 4) = "1000";
        v := (n => '1', others => '0');
        assert v = "0010";
        wait;
    end process;
end architecture;
