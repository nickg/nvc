entity signal2 is
end entity;

architecture test of signal2 is
    signal x : bit := '0';
begin

    p1: process is
    begin
        assert x'event;
        assert x'active;
        wait;
    end process;

end architecture;
