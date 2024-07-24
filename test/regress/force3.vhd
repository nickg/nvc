entity force3 is
end entity;

architecture test of force3 is
    signal s : bit_vector(1 to 3);

    function get_val return bit_vector is
    begin
        return "111111111";
    end function;
begin

    s <= "111";

    process is
    begin
        s <= force "101";
        wait for 0 ns;
        assert s = "101";
        s <= force get_val;             -- Error
        wait;
    end process;

end architecture;
