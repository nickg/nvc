package ename is
    procedure test_lookup;
end package;

package body ename is
    constant c : integer := 42;

    procedure test_lookup is
    begin
        assert << constant @work.ename.c : integer >> = 42;
    end procedure;
end package body;
