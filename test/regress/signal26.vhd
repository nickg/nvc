entity signal26 is
end entity;

architecture test of signal26 is
    function func (x : integer) return integer is
    begin
        return x / 2;
    end function;

    constant w : integer := 4;

    type rec is record
        f : bit_vector(func(w) - 1 downto 0);
    end record;

    signal v : bit_vector(w - 1 downto 0);
    signal r : rec;
begin

    v(w-1 downto r.f'left + 1) <= (others => '1');
    v(r.f'left downto 0) <= (others => '0');

    check: process is
    begin
        wait for 1 ns;
        assert v = "1100";
        wait;
    end process;

end architecture;
