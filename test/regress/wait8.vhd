entity wait8 is
end entity;

architecture test of wait8 is

    procedure foo(signal x : in bit_vector) is
    begin
        wait on x;
    end procedure;

    signal s : bit_vector(1 to 2) := "00";

begin

    a: process is
    begin
        foo(s);
        assert now = 10 ns;
        assert s = "11";
        wait;
    end process;

    b: process is
    begin
        s <= "11" after 10 ns;
        wait;
    end process;

end architecture;
