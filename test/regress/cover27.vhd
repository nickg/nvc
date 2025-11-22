library ieee;
use ieee.std_logic_1164.all;

entity cover27 is
generic (
    G_PAR : integer
);
end entity;

architecture test of cover27 is

    signal a,b,c,d,e,f : std_logic;
    signal vec : std_logic_vector(3 downto 0);

begin

    G_IF_GEN : if (G_PAR = 0) generate
        p1: a <= '0';
    elsif G_IF_GEN_1:  (G_PAR = 1) generate
        p2: b <= '0';
    end generate;

    T_FOR_GEN : for i in 0 to 3 generate
        p3: vec(i) <= a when (i = 0) else
                      b when (i = 1) else
                      c when (i = 2) else
                      d;
    end generate;

    G_CASE_GEN : case (G_PAR) generate
    when G_CASE_0: 0 => p4: c <= '1';
    when G_CASE_1: 1 => p5: d <= '0';
    end generate;

end architecture;
