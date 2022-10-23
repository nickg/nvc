entity array1 is
end entity;

architecture test of array1 is

    impure function func return bit_vector is
    begin
        return "101";
    end function;

begin

    p1: process is
    begin
        assert func = "10";
        wait;
    end process;

end architecture;
