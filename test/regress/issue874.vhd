entity issue874 is
end entity;

architecture arch of issue874 is
    constant A : natural := 15;
    constant B : natural := A-1;
    type array1 is array (0 to 3) of bit_vector(A-1 downto 0);
    signal s1 : array1 := (others => (others => '0'));
    type array2 is array (0 to 3) of bit_vector;
    signal s2 : array2(open)(B downto 0) := (others => (others => '0'));
begin
    process
    begin
        assert s1 = (0 to 3 => (14 downto 0 => '0'));  -- Crash
        assert s1 = (0 to 3 => (B downto 0 => '0'));  -- Crash
        assert s1 = (0 to 3 => (A-1 downto 0 => '0'));  -- Crash
        assert s2 = (0 to 3 => (A-1 downto 0 => '0'));  -- Crash
        wait;
    end process;
end architecture arch;
