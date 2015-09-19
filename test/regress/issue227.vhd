entity issue227 is
end entity issue227;
use std.textio.all;
architecture test of issue227 is
    procedure proc(
        delay : time    := 0 ns;
        a,b   : integer := 1) is
    begin
        wait for delay;
        write(OUTPUT, integer'image(a) & integer'image(b) & LF);
    end procedure proc;

    function func(a,b : integer := 1) return integer is
    begin
        return 10*a+b;
    end function func;

begin

    proc;                           -- all unassociated
    proc(1 ns,    2, b => open);    -- named OPEN
    proc(2 ns, open, b => 2);       -- positional OPEN
    proc(3 ns, a => 2);             -- one unassociated

    process
    begin
        wait for 4 ns;
        proc;                       -- all unassociated
        proc(0 ns, 2, b => open);   -- named OPEN
        proc(0 ns, open, b => 2);   -- positional OPEN
        proc(0 ns, a => 2);         -- one unassociated

        write(OUTPUT, integer'image(func) & LF);
        write(OUTPUT, integer'image(func(a => 2, b => open)) & LF);
        write(OUTPUT, integer'image(func(open, b => 2)) & LF);
        write(OUTPUT, integer'image(func(a => 2)) & LF);
        wait;
    end process;

end architecture test;
