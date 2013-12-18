entity bounds16 is
end entity;

architecture test of bounds16 is

    function add_nat(x : in integer) return natural is
        variable r : natural := x;
    begin
        return r + 1;
    end function;

begin

    process is
        variable x : integer;
    begin
        x := 5;
        report integer'image(add_nat(x));
        x := 0;
        report integer'image(add_nat(x));
        x := -1;
        report integer'image(add_nat(x));  -- Error
        wait;
    end process;

end architecture;
