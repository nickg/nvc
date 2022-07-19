package pack is
    type t is (foo, bar, baz);

    procedure foo(x : integer);
    procedure bar;
    procedure baz(x : t);
end package;

-------------------------------------------------------------------------------

entity names3 is
end entity;

use work.pack;

architecture test of names3 is
begin

    p1: process is
    begin
        bar;                            -- Error
        pack.bar;                       -- OK
        pack.foo(1);                    -- OK
        pack.baz(pack.baz);             -- OK
        wait;
    end process;

    p2: process is
        function f1(x : real) return string;
        function f1 return string;
        function f1(x : integer) return string;
    begin
        report f1;                      -- OK
    end process;

end architecture;
