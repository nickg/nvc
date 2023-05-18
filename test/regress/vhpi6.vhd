entity vhpi6 is
    port (
        x : in natural;
        y : out natural );
end entity;

architecture test of vhpi6 is
begin

    p1: process (x) is
    begin
        report "x=" & integer'image(x);
        y <= x + 1 after 1 ns;
    end process;

    p2: process is
    begin
        wait for 1 ms;
        report "VHPI plugin did not end simulation" severity failure;
    end process;

end architecture;
