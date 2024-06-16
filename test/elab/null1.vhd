entity null4 is
    generic (g : integer := -1);
end entity;

architecture test of null4 is
    subtype t_null_int is integer range 0 to g;
    signal x : t_null_int;              -- Error
begin
end architecture;
