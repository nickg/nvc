entity sub is
    generic (
        X : integer );
end entity;

architecture test of sub is
    signal s : bit_vector(7 downto 0);
begin

    gen: if x >= s'low and x <= s'high generate
        s(x) <= '1';
    end generate;

end architecture;

-------------------------------------------------------------------------------

entity gbounds is
end entity;

architecture test of gbounds is
begin

    sub1: entity work.sub generic map ( 2 );  -- OK
    sub2: entity work.sub generic map ( 9 );  -- No error

end architecture;
