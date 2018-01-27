package test is

        function x(i : integer) return integer;

end package;

package body test is

        function x(i : integer) return integer is
                variable x : integer;
        begin
                x := i + 5;             -- Spurious error here
                return x;
        end function;

        procedure y is
            variable y : integer;       -- Spurious error here
        begin
        end procedure;

end package body;
