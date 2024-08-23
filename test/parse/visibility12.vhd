-- work library is mylib
package mylib is
  constant C_CONSTANT : integer := 2;
end package;

entity cntr is
    port ( x : in integer );
end entity;

library mylib;
use mylib.mylib.all;

entity e1 is
end entity;

architecture a1 of e1 is
begin
    u: entity mylib.cntr port map ( C_CONSTANT);  -- OK
end architecture;

library mylib;
use mylib.mylib;

entity e2 is
end entity;

architecture a2 of e2 is
begin
    u: entity mylib.cntr port map ( mylib.C_CONSTANT);  -- Error
end architecture;
