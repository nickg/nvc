entity sub is
    port (
        i : in bit_vector(3 downto 0);
        o : out bit_vector(3 downto 0) );
end entity;

architecture test of sub is
begin
    o(1 downto 0) <= "00";              -- Crash when O not in port map
    o(3 downto 2) <= i(2 downto 1);
end architecture;

entity top is
end entity;

architecture test of top is
    signal x : bit_vector(3 downto 0);
begin

    sub_i: entity work.sub port map ( x );

end architecture;
