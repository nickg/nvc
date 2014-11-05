package pack is

    type SharedCounter is protected
        procedure increment (N: Integer := 1);
        procedure decrement (N: Integer := 1);
        impure function value return Integer;
    end protected SharedCounter;

end package;

package body pack is

    type SharedCounter is protected body
        constant INIT : integer := 5;
        variable counter: Integer := INIT;
        variable dummy: Integer;

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

-------------------------------------------------------------------------------

entity protected2 is
end entity;

use work.pack.all;

architecture test of protected2 is
    shared variable x : SharedCounter;
begin

    process is
    begin
        assert x.value = 5;
        x.increment;
        report "value is now " & integer'image(x.value);
        x.increment(2);
        assert x.value = 8;
        wait;
    end process;

    process is
    begin
        wait for 1 ns;
        assert x.value = 8;
        x.decrement;
        assert x.value = 7;
        wait;
    end process;

end architecture;
