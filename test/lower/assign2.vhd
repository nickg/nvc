entity assign2 is
end entity;

architecture test of assign2 is
begin

    p1: process is
        variable x : bit_vector(7 downto 0) := (1 => '1', others => '0');
        subtype myint is integer range 1 to 10;
        type myint_array is array (integer range <>) of myint;
        variable y : myint_array(1 to 3);
    begin
        assert x(0) = '0';
        assert x(4) = x(7);
        x(2) := '1';
        y(1) := y(3);
        wait;
    end process;

end architecture;
