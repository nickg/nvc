entity error3 is
end entity;

architecture test of error3 is
    -- Examples from LRM 93 section 7.2.5

    type R1 is 0 to 7;                  -- Error
    type R2 is 7 downto 0;              -- Error

    type T1 is array (R1 range <>) of Bit;
    type T2 is array (R2 range <>) of Bit;

    subtype S1 is T1(R1);
    subtype S2 is T2(R2);

    constant K1: S1 := (others => '0');
    constant K2: T1 := K1(1 to 3) & K1(3 to 4); -- K2'Left = 0 and K2'Right = 4
    constant K3: T1 := K1(5 to 7) & K1(1 to 2); -- K3'Left = 0 and K3'Right = 4
    constant K4: T1 := K1(2 to 1) & K1(1 to 2); -- K4'Left = 0 and K4'Right = 1
    constant K5: S2 := (others => '0');
    constant K6: T2 := K5(3 downto 1) & K5(4 downto 3); -- K6'Left = 7 and K6'Right = 3
    constant K7: T2 := K5(7 downto 5) & K5(2 downto 1); -- K7'Left = 7 and K7'Right = 3
    constant K8: T2 := K5(1 downto 2) & K5(2 downto 1); -- K8'Left = 7 and K8'Right = 6
begin

end architecture;
