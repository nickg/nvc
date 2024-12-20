entity guard5 is
end entity;

architecture test of guard5 is

    type int_vec is array (natural range <>) of integer;

    function resolved (x : int_vec) return integer is
    begin
        return x'length;
    end function;

    subtype rint is resolved integer;

    signal vbus : rint bus;
begin

    b: block is
        port (
            pbus : inout rint bus );
        port map ( vbus );
    begin

        check: process is
        begin
            assert pbus = 1;
            assert pbus'driving;
            pbus <= 100;
            wait for 1 ns;
            assert pbus = 1;
            pbus <= null;
            wait for 1 ns;
            assert pbus = 1 severity warning;  -- XXX: wrong
            assert not pbus'driving;
            wait;
        end process;
    end block;

end architecture;
