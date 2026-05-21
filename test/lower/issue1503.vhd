package pkg1 is
    generic (a: natural);
end package;

package pkg1_inst is new work.pkg1 generic map (a => 2);

package pkg2 is
    generic (
        package pkg1_inst_sub is new work.pkg1 generic map (<>);
        -- If this is removed, nvc appears to get stuck and never exit
        -- (killed it after >1 min of no output)
        constant b: natural
    );
end package;

package pkg2_inst is new work.pkg2 generic map (
    pkg1_inst_sub => work.pkg1_inst,
    b => 0
);

entity Sub is
    generic (
        package pkg2_inst_sub is new work.pkg2 generic map (<>)
    );
end;

architecture rtl of Sub is
    package foo is new work.pkg1 generic map (
        a => pkg2_inst_sub.pkg1_inst_sub.a
    );
begin
end;

entity issue1503 is
end;

architecture rtl of issue1503 is
begin
    sub: entity work.Sub generic map (work.pkg2_inst);
end;
