entity issue1029 is
end entity;

architecture test of issue1029 is

    procedure p (signal x : inout bit) is
    begin
        wait for 1 ns;
    end procedure;

    signal s : bit;

begin

    call: p(<< signal .issue1029.s : bit >>);  -- Crash here

end architecture;
