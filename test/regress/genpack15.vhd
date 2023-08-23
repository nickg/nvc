package generic_array_pkg is
    generic ( type t;
              size : natural );

    type t_array is protected
        impure function get (pos : natural) return t;
        procedure set (pos : natural; value : t);
    end protected;
end package;

package body generic_array_pkg is
    type t_array is protected body
        type t_backing is array (1 to size) of t;
        variable data : t_backing;

        impure function get (pos : natural) return t is
        begin
            return data(pos);
        end function;

        procedure set (pos : natural; value : t) is
        begin
            data(pos) := value;
        end procedure;
    end protected body;
end package body;

-------------------------------------------------------------------------------

package generic_stack_pkg is
    generic ( type t;
              size : natural );

    type t_stack is protected
        impure function pop return t;
        procedure push (value : t);
    end protected;
end package;

package body generic_stack_pkg is
    package array_pkg is new work.generic_array_pkg
        generic map ( t => t, size => size );

    type t_stack is protected body
        variable count : natural;
        variable data : array_pkg.t_array;

        impure function pop return t is
            constant pos : positive := count;
        begin
            count := count - 1;
            return data.get(pos);
        end function;

        procedure push (value : t) is
        begin
            count := count + 1;
            data.set(count, value);
        end procedure;
    end protected body;
end package body;

-------------------------------------------------------------------------------

entity genpack15 is
end entity;

architecture test of genpack15 is
    package stack_pkg is new work.generic_stack_pkg
        generic map ( t => integer, size => 8 );

    shared variable stack : stack_pkg.t_stack;
begin

    tb: process is
    begin
        stack.push(5);
        stack.push(6);
        stack.push(7);
        wait for 1 ns;
        assert stack.pop = 7;
        assert stack.pop = 6;
        assert stack.pop = 5;
        wait;
    end process;

end architecture;
