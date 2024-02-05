entity AliasRangeExample is
end entity AliasRangeExample;

architecture Bhv of AliasRangeExample is

    signal A: bit_vector(31 downto 0);
    signal B,C,D: bit_vector(7 downto 0);

    subtype aRange is natural range 7 downto 0;
    alias aRangeAlias is aRange;
begin
    B <= A(aRange); -- okay
    C <= A(aRangeAlias'range); -- okay
    D <= A(aRangeAlias); -- ** Error: invalid use of alias ARANGEALIAS
end architecture;
