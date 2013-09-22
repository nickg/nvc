entity arraycase is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture test of arraycase is
    constant LOOPS : integer := 100;
begin

    process is
        variable vec : unsigned(15 downto 0) := (others => '0');
        variable a, b, c, d, e, f : natural;
    begin
        for l in 1 to LOOPS loop
            for i in 0 to integer'((2 ** 16) - 1) loop
                vec := vec + X"0001";
                case vec is
                    when X"0005" | X"ac3d" | X"9141" | X"2562" | X"0001" =>
                        a := a + 1;
                    when X"5101" | X"bbbb" | X"cccc" | X"dddd" =>
                        b := b + 1;
                    when X"0000" | X"ffff" =>
                        c := c + 1;
                    when X"2510" | X"1510" | X"babc" | X"aaad" | X"1591" =>
                        d := d + 1;
                    when X"9151" | X"abfd" | X"ab41" =>
                        e := e + 1;
                    when X"1111" | X"9150" =>
                        f := f + 1;
                    when others =>
                        null;
                end case;
                wait for 1 ns;
            end loop;
        end loop;

        report integer'image(a) & " " & integer'image(b) & " "
            & integer'image(c) & " " & integer'image(d) & " "
            & integer'image(e) & " " & integer'image(f);

        wait;
    end process;

end architecture;
