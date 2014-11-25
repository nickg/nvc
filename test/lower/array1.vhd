entity array1 is
end entity;

architecture test of array1 is

    function func return bit_vector;

begin

    process is
    begin
        assert func = "10";
        wait;
    end process;

end architecture;
