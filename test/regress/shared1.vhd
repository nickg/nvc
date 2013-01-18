package p is

    shared variable pglobal : integer := 2;

end package;

-------------------------------------------------------------------------------

entity shared1 is
end entity;

use work.p.all;

architecture test of shared1 is

    shared variable global : integer := 5;

    procedure check_it(constant expect : in integer) is
    begin
        assert global = expect;
    end procedure;

    procedure set_it(constant set : in integer) is
    begin
        global := set;
    end procedure;

begin

    process is
    begin
        assert global = 5;
        global := 6;
        check_it(6);
        set_it(7);
        assert global = 7;

        assert pglobal = 2;
        pglobal := 51;
        wait;
    end process;

end architecture;
