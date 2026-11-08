-- Nested procedures with an empty signature in a generic package must
-- each run their own body. See issue #1740.

package issue1740_pkg is
    generic (type t);
    procedure run(constant n : t);
end package;

package body issue1740_pkg is
    procedure run(constant n : t) is
        variable seen  : integer := 0;
        variable trace : integer := 0;

        procedure first is
        begin
            seen := 1;
        end procedure;

        procedure second is
            procedure deeper is
            begin
                seen := 4;
            end procedure;
        begin
            deeper;
            seen := seen + 2;
        end procedure;

        procedure third is
        begin
            seen := 3;
        end procedure;

        procedure fourth(constant k : integer := 5) is
        begin
            seen := k;
        end procedure;
    begin
        first;
        trace := trace * 10 + seen;
        second;
        trace := trace * 10 + seen;
        third;
        trace := trace * 10 + seen;
        fourth;
        trace := trace * 10 + seen;

        -- 1635 when each call runs its own body. 1115 when every empty
        -- procedure runs the body of first.
        assert trace = 1635
            report "trace " & integer'image(trace)
            severity failure;
    end procedure;
end package body;

entity issue1740 is
end entity;

architecture test of issue1740 is
    package dut is new work.issue1740_pkg generic map (t => integer);
begin
    process is
    begin
        dut.run(0);
        wait;
    end process;
end architecture;
