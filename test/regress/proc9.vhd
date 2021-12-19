entity proc9 is
end entity;

architecture test of proc9 is

    procedure foo (x : in integer; y : out bit_vector) is
        constant c : bit_vector(1 to x) := (others => '1');
    begin
        wait for 1 ns;
        y := c;
    end procedure;

begin

    process is
        variable b : bit_vector(1 to 5);
    begin
        foo(5, b);
        assert b = (1 to 5 => '1');
        wait;
    end process;

end architecture;
