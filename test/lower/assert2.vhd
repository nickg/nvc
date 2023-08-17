entity assert2 is
end entity;

architecture test of assert2 is
    procedure test(x : integer; s : string) is
    begin
        assert x > 0 report "s=" & s;   -- Message has allocation side-effect
    end procedure;
begin

    p1: process is
        variable x : integer;
    begin
        test(-6, "hello");
        wait;
    end process;

end architecture;
