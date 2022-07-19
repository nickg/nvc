package gen_counter is
    generic ( type t;
              one, zero : t;
              function "+"(x, y : t) return t is <>;
              function "-"(x, y : t) return t is <> );

    type counter is protected
        procedure increment;
        procedure decrement;
        impure function value return t;
    end protected;

    shared variable var : counter;

end package;

package body gen_counter is

    type counter is protected body
        variable val : t := zero;

        procedure increment is
        begin
            val := val + one;
        end procedure;

        procedure decrement is
        begin
            val := val - one;
        end procedure;

        impure function value return t is
        begin
            return val;
        end function;
    end protected body;

end package body;

-------------------------------------------------------------------------------

package int_counter is new work.gen_counter
    generic map ( t => integer, one => 1, zero => 0 );

-------------------------------------------------------------------------------

use work.int_counter;

entity genpack8 is
end entity;

architecture test of genpack8 is

    package real_counter is new work.gen_counter
        generic map (t => real, one => 1.0, zero => 0.0 );

begin

    main: process is
    begin
        assert int_counter.var.value = 0;
        int_counter.var.increment;
        int_counter.var.increment;
        int_counter.var.decrement;
        assert int_counter.var.value = 1;

        assert real_counter.var.value = 0.0;
        real_counter.var.increment;
        real_counter.var.increment;
        real_counter.var.decrement;
        assert real_counter.var.value = 1.0;

        wait;
    end process;

end architecture;
