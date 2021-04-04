package issueH is
    constant c : bit_vector;
end package issueH;

package body issueH is
    pure function f (
        i : bit_vector;
        l : integer range 1 to integer'high
    ) return bit_vector is
        variable v : bit_vector(i'length-1 downto 0);
    begin
        v := i;
        return v(l-1 downto 0);
    end function f;
    constant c : bit_vector := f(X"1F", 5);
end package body issueH;

entity issue57 is
end entity;

use work.issueH.all;

architecture test of issue57 is
begin

    process is
    begin
        assert not c'ascending;
        assert c'length = 5;
        assert c = "11111";
        wait;
    end process;

end architecture;
