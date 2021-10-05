entity driving is
    port ( i : in integer;
           o : out integer );
end entity;

architecture test of driving is
    signal x : integer;
begin

    p1: process is
        variable v : integer;
    begin
        x <= 1;
        assert x'driving;               -- OK
        assert v'driving;               -- Error
        assert o'driving;               -- OK
        assert i'driving;               -- Error
        wait;
    end process;

end architecture;
