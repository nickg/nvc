package pack is
    function func(x : integer) return integer;
    procedure proc(x : integer);
end package;

-------------------------------------------------------------------------------

entity duplicate is
end entity;

use work.pack.all;

architecture test of duplicate is
    function func(x : integer) return integer is
    begin
        return x + 1;
    end function;

    procedure proc(x : integer) is
    begin
    end procedure;
begin

    process is
    begin
        assert func(2) = 3;             -- OK
        assert pack.func(2) = 3;        -- OK
        proc(2);                        -- OK
        pack.proc(2);                   -- OK
        wait;
    end process;

end architecture;
