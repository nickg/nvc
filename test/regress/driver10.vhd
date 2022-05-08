package pack is
    type op_t is (IDLE, DO_A, DO_B, CLASH);
    type op_vec_t is array (natural range <>) of op_t;

    function resolved (s : op_vec_t) return op_t;

    subtype r_op_t is resolved op_t;
end package;

package body pack is

    function resolved (s : op_vec_t) return op_t is
        variable result : op_t := IDLE;
    begin
        for i in s'range loop
            if result = IDLE then
                result := s(i);
            elsif s(i) /= IDLE then
                result := CLASH;
                exit;
            end if;
        end loop;
        report "result=" & op_t'image(result);
        return result;
    end function;

end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( s : inout r_op_t;
           done : out boolean );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        assert s = idle;
        wait on s;
        assert now = 1 ns;
        assert s = do_a;
        wait on s;
        assert now = 2 ns;
        assert s = do_b;
        done <= true;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity driver10 is
end entity;

architecture test of driver10 is
    signal t    : r_op_t := IDLE;
    signal done : boolean := false;
begin

    uut: entity work.sub port map (t, done);

    p2: process is
    begin
        assert t = IDLE;
        wait for 1 ns;
        t <= DO_A;
        wait for 1 ns;
        t <= DO_B;

        wait;
    end process;

    p3: process is
    begin
        wait for 1 hr;
        assert done = true;
        wait;
    end process;

end architecture;
