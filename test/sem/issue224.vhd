entity test is
end entity test;

architecture test of test is
    -- next line should fail to compile because it's not legal to assign a default value to a signal parameter
    procedure proc(signal a : integer := 1) is
    begin
    end procedure proc;
begin
end architecture test;

architecture test2 of test is
    procedure proc(signal a : integer) is
    begin
    end procedure proc;
begin
    -- next line should also fail to compile because it's not legal to have no actual or an OPEN actual
    proc(a => open);
end architecture test2;

architecture test3 of test is
    procedure proc(
        variable a : in    integer := 1;
        variable a : out   integer := 1
    ) is
    begin
    end procedure proc;
begin
end architecture test3;

architecture test4 of test is
    procedure proc(
        variable a : in    integer := 1;
        variable a : inout integer := 1
    ) is
    begin
    end procedure proc;
begin
end architecture test4;
