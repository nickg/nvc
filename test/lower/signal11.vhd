package pack is
    signal x : integer := 0;

    procedure inc(signal s : out integer);
end package;

package body pack is

    procedure inc(signal s : out integer) is
    begin
        s <= x + 1;
    end procedure;

    procedure bar(signal s : in integer) is
    begin
    end procedure;

    procedure foo is
    begin
        bar(x);
    end procedure;

end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
end entity;

architecture test of sub is
begin

    process is
    begin
        report x'path_name;
        report x'instance_name;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity signal11 is
end entity;

use work.pack.all;

architecture test of signal11 is
begin

    process is
    begin
        assert x = 0;
        inc(x);
        wait for 1 ns;
        assert x = 1;
        wait;
    end process;

    sub_i: entity work.sub;

end architecture;
