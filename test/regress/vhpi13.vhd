entity vhpi13 is
end entity;

architecture test of vhpi13 is
    signal seq : natural;
begin

    seq <= 1 after 1 ns, 2 after 2 ns, 3 after 3 ns;

end architecture;
