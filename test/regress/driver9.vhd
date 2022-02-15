entity driver9 is
end entity;

architecture test of driver9 is
    signal x : integer;
begin

    x <= 1;
    x <= 2;             -- Error, this used to be detected statically

end architecture;
