entity bounds24 is
end entity;

architecture test of bounds24 is

    function func (n : natural) return bit is
        variable r : bit_vector(1 to 3) := (1 to n => '1');
    begin
        return r(1) xor r(2) xor r(3);
    end function;

    signal n : integer := 3;
begin

    main: process is
    begin
        assert func(3) = '1';           -- OK
        assert func(n) = '1';           -- OK
        n <= 1000;
        wait for 1 ns;
        assert func(n) = '1';      -- Error
        wait;
    end process;

end architecture;
