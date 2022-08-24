entity altera1 is
    generic (g : integer);
    type t is array (1 to 2**g) of bit;
end entity;

architecture a of altera1 is
    constant c : t := (others => '0');  -- Crash here
begin
end architecture;
