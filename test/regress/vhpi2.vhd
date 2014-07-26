entity vhpi2 is
    port (
        x : in natural;
        y : out natural );
end entity;

architecture test of vhpi2 is
begin

    process (x) is
    begin
        report "x=" & integer'image(x);
        y <= x + 1 after 1 ns;
    end process;

end architecture;
