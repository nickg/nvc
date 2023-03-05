entity signal30 is
end entity;

architecture test of signal30 is

    procedure proc (signal x : inout bit_vector) is
    begin
        x <= not x;
        wait for 1 ns;
    end procedure;

    signal s : bit := '0';
begin

    p1: process is
    begin
        proc(x(0) => s);
        assert s = '1';
        proc(x(6) => s);
        assert s = '0';
        wait;
    end process;

end architecture;
