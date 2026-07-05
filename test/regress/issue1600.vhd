package pkg is
  type int_array is array (0 to 0) of integer;
  constant CFG : int_array := (others => 5);
  -- Indexed name => not locally static in VHDL-93/2002, so strict analysis
  -- leaves NAHBIRQ unfolded in the package.
  constant NAHBIRQ : integer := CFG(0) + 32;
  component duv is
    generic ( g : integer := NAHBIRQ );
  end component;
end package;

entity duv is
  generic ( g : integer );
end;

architecture rtl of duv is
begin
  assert g = 37;
end;

use work.pkg.all;

entity issue1600 is
end entity;

architecture behav of issue1600 is
begin
  u: duv;  -- component instantiation, default binding
end architecture;
