entity afunc is
end entity;

architecture test of afunc is

    function get return bit_vector is
    begin
        return X"10";
    end function;

begin

    process is
    begin
        assert get(0) = '0';            -- OK
        wait;
    end process;

end architecture;
