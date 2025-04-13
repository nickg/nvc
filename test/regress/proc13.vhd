package pack is
    constant def : integer;
    procedure test1 (x : integer := def);
    procedure test2 (x : integer := 1 / 0);

    type pt is protected
        procedure test3 (x : integer := def);
    end protected;
end package;

package body pack is
    constant def : integer := 42;

    procedure test1 (x : integer := def) is
    begin
        wait for 1 ns;
    end procedure;

    procedure test2 (x : integer := 1 / 0) is
    begin
        wait for 1 ns;
    end procedure;

    type pt is protected body
        procedure test3 (x : integer := def) is
        begin
        end procedure;
    end protected body;
end package body;

-------------------------------------------------------------------------------

entity proc13 is
end entity;

use work.pack.all;

architecture test of proc13 is
    shared variable sv : pt;
begin

    main: process is
    begin
        test1;
        sv.test3;
        test2;                          -- Error
        wait;
    end process;

end architecture;
