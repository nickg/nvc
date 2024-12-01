package my_pkg is

    signal sig_in_package : natural := 17;

end package;

entity ename16 is
end entity;

architecture test of ename16 is
begin

    process
        alias al is << signal @work.my_pkg.sig_in_package : natural >>;
    begin
        wait for 1 ns;
        assert (al = 17);
        al <= 5;
        wait for 1 ns;
        assert al = 5;
        wait for 1 ns;

        assert << signal @work.my_pkgxxxx.sig : natural >> = 1;  -- Error
        wait;
    end process;

end architecture;
