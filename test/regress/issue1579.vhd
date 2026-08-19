entity issue1579 is
end entity;

architecture test of issue1579 is
    function add2(x : integer) return integer is
    begin
        assert false report "foreign binding was not resolved" severity failure;
    end function;

    attribute foreign : string;
    attribute foreign of add2 : function is "VHPI issue1579 add2";
begin
    process is
    begin
        assert add2(1) = 3;
        wait;
    end process;
end architecture;
