entity vhdl1 is
    port ( x : in bit;
           y : out integer );
end entity;

architecture test of vhdl1 is
    signal z : bit;
begin
    z <= x after 1 ns;
end architecture;
