entity attr4 is
end entity;

architecture test of attr4 is
begin

    process is
        variable b : boolean;
    begin
        assert boolean'pos(false) = 0;
        assert boolean'pos(true) = 1;
        b := true;
        wait for 1 ns;
        assert boolean'pos(b) = 1;
        assert boolean'val(0) = false;
        assert bit'val(1) = '1';
        wait;
    end process;

end architecture;
