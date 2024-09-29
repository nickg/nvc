entity issue988 is
end entity;

architecture test of issue988 is
    function init return integer is
    begin
        report "should not call this" severity failure;
    end function;

    attribute foreign of init : function is "VHPI issue988 init";

    function init_direct return integer is
    begin
        report "should not call this" severity failure;
    end function;

    attribute foreign of init_direct : function is "VHPIDIRECT __vhpi_init_direct";

    constant c1 : integer := init;
    constant c2 : integer := init_direct;
begin

    check: process is
    begin
        assert c1 = 42;
        assert c2 = 66;
        wait;
    end process;

end architecture;
