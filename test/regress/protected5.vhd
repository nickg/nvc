-- The order of design units in the file is significant

package pack1 is

    type SharedCounter is protected
        procedure increment (N: Integer := 1);
        procedure decrement (N: Integer := 1);
        impure function value return Integer;
    end protected SharedCounter;

end package;

-------------------------------------------------------------------------------

use work.pack1.all;

package pack2 is

    shared variable sv : SharedCounter;

end package;

-------------------------------------------------------------------------------

entity protected5 is
end entity;

use work.pack2.all;

architecture test of protected5 is
begin

    p1: process is
    begin
        sv.increment;
        sv.increment;
        wait for 0 ns;
        assert sv.value = 3;
        wait;
    end process;

    p2: process is
    begin
        sv.increment;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

package body pack1 is

    type SharedCounter is protected body
        variable counter: Integer := 0;

        procedure increment (N: Integer := 1) is
        begin
            counter := counter + N;
        end procedure increment;

        procedure decrement (N: Integer := 1) is
        begin
            counter := counter - N;
        end procedure decrement;

        impure function value return Integer is
        begin
            return counter;
        end function value;
    end protected body;

end package body;
