entity vhpi1 is
    port (
        x : in natural;
        y : out natural );
end entity;

architecture test of vhpi1 is
    subtype zero_to_one is real range 0.0 to 1.0;
    signal v : bit_vector(3 downto 0) := "0011";
    signal b : bit;
    signal r : zero_to_one;
    signal i : integer := 42;
    signal A_name_with_MIXED_case : bit;
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
