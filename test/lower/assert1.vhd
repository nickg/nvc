entity assert1 is
end entity;

architecture test of assert1 is
begin

    p1: process is
        variable b : boolean;
    begin
        b := true;
        assert b;                       -- Should be optimised in vcode
        wait;
    end process;

end architecture;
