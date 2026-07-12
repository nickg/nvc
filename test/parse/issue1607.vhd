entity X is
  generic ( A : integer; B : bit );
end entity;
architecture rtl of X is
begin
end architecture;

entity Y is
  generic ( B : integer );
end entity;
architecture rtl of Y is
  constant C : integer := 2 when B = 7 else 1;    -- works
  constant V : bit_vector(1 to 4) := X"1";
begin
  I: entity work.x generic map ( A => 2 when B = 7 else 1,     -- fails
                                 V(C + 1) when B = 2 else '1' );
end architecture;
