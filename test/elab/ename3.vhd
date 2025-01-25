package pack is
    constant kk : integer := <<constant .ename3.k : integer>>;  -- OK (since 1.16)
end package;

-------------------------------------------------------------------------------

entity ename3 is
end entity;

use work.pack.all;

architecture test of ename3 is
    constant k : integer := 42;
begin
end architecture;
