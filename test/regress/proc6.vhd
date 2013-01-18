entity proc6 is
end entity;

architecture test of proc6 is

    procedure delay(signal x   : out integer;
                    signal y   : in integer;
                    constant d : in delay_length) is
    begin
        x <= y after d;
    end procedure;

    signal a, b : integer;

begin

    foo: delay(a, b, 10 ns);

    check: process is
    begin
        b <= 6;
        wait for 11 ns;
        assert a = 6;
        b <= 7;
        wait for 5 ns;
        assert a = 6;
        wait for 5 ns;
        assert a = 7;
        wait;
    end process;

end architecture;
