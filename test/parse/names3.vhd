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

    p3: process is
        type byte_vector is array (1 to 8) of bit_vector(1 to 8);
        variable v : bit_vector(7 downto 0);
    begin
        if v(7 downto 3) & "000" = X"12" then  -- OK
        end if;
    end process;

end architecture;
