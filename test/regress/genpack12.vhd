package sigpack is
    generic (width : positive);
    signal s : bit_vector(1 to width);
    signal t : integer;
end package;

entity genpack12 is
end entity;

package p1 is new work.sigpack generic map (3);

use work.p1.all;

architecture test of genpack12 is
    package p2 is new work.sigpack generic map (4);
    package p3 is new work.sigpack generic map (5);
begin

    s <= "110";
    t <= 5;

    p2.s <= "1010";
    p2.t <= 7;

    p3.s <= "11001";
    p3.t <= 3;

    check: process is
    begin
        wait for 1 ns;
        assert s = "110";
        assert p2.s = "1010";
        assert p3.s = "11001";
        assert t = 5;
        assert p2.t = 7;
        assert p3.t = 3;
        wait;
    end process;

end architecture;
