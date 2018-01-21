package r is

    function r1(a:bit_vector) return bit_vector;

end package;

package body r is

    function r1(a:bit_vector) return bit_vector is
        variable ret : bit_vector(a'range);
        variable i : integer range a'range;  -- Error here
    begin
        loop
            report integer'image(i);
            ret(i) := not a(i);
            exit when i = i'right;
            i := i + 1;
        end loop;

        return ret;
    end r1;

end r;

entity issue367 is
end entity;

use work.r.r1;

architecture test of issue367 is
begin

    process is
        variable b : bit_vector(1 to 3) := "101";
    begin
        assert r1(b) = "010";
        wait;
    end process;

end architecture;
