entity vhpi16 is
end entity;

architecture test of vhpi16 is
    signal x : integer;
begin

    x <= 1 after 1 ns, 2 after 2 ns, 3 after 3 ns;

end architecture;
