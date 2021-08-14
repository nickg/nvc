entity sub is
    port (
        result : out bit_vector(3 downto 0);
        in1    : in  bit_vector(3 downto 0) );
end entity;

architecture test of sub is
    signal in2 : bit_vector(2 downto 0);
begin

    assert in1(1 downto 0) = "00";

    in2 <= "001";
    result <= '0' & bit_vector(in1 and in2);

end architecture;

-------------------------------------------------------------------------------

entity partial1 is
end entity;

architecture test of partial1 is
    signal result : bit_vector(3 downto 0);
    signal in1    : bit_vector(1 downto 0);
begin

    uut: entity work.sub
        port map (
            result => result,
            in1(3 downto 2) => bit_vector(in1),
            in1(1 downto 0) => "00" );

    stim: process is
    begin
        in1 <= "01";
        wait for 1 ns;
        assert result = X"5";
        wait;
    end process;

end architecture;
