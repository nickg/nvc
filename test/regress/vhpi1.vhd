entity vhpi1 is
    port (
        x : in natural;
        y : out natural );
end entity;

architecture test of vhpi1 is
    signal v : bit_vector(3 downto 0) := "0011";
    signal b : bit;
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
