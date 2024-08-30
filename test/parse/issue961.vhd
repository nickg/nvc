entity issue961 is
end entity;

architecture test of issue961 is
    function get_val return real is
    begin
        return 1.234;
    end function;

    function get_val return integer is
    begin
        return 1234;
    end function;

    function rand_val(n : integer) return real is
    begin
        return 1.234;
    end function;

    function rand_val(n : integer) return integer is
    begin
        return 1234;
    end function;
begin

    process is
    begin
        for i in 0 to 5 + get_val loop  -- OK
        end loop;
        for i in 0 to 2**5 + rand_val(1) loop  -- OK
        end loop;
        wait;
    end process;

end architecture;
