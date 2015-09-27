entity issue239 is
end entity;
architecture test of issue239 is

    shared variable sv  : boolean := false;
    constant cb         : boolean := sv;
    signal sb           : boolean := sv;        -- ok

begin

process
    variable var  : boolean := false;
    variable var2 : boolean := var;             -- ok

    procedure proc(
        constant b : in boolean := var          -- error
    ) is
    begin
        report boolean'image(b) severity note;
    end procedure;

    procedure proc2 (
        variable b : in boolean := var          -- error
    ) is
    begin
        report boolean'image(b) severity note;
    end procedure;

begin
    proc;
    var := true;
    proc;
    wait;
end process;
end architecture;

package package_issue239 is
    constant deferred : integer;

    procedure proc(a : integer := deferred);    -- OK

end package;

