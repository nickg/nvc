library ieee;
use ieee.std_logic_1164.all;

entity cover20 is
    generic (
        G_VAL : integer
    );
end cover20;

architecture test of cover20 is

    signal a : std_logic;
    signal b : std_logic;

    -- Toggle is within the same scope.
    -- This tests that merging puts tgl(1) from second run
    -- to the merged DB
    signal tgl : std_logic_vector(G_VAL downto 0);

begin

    gen_zero: if (G_VAL = 0) generate
    begin
        process
        begin
            a <= '1';
            wait for 1 ns;
            a <= '0';
            wait for 1 ns;

            tgl(0) <= '1';
            wait for 1 ns;
            tgl(0) <= '0';
            wait for 1 ns;
            tgl(0) <= '1';
            wait for 1 ns;

            wait;
        end process;

    end generate;

    -- Generate causes new coverage scope -> Merge of whole scope is tested.
    gen_one: if (G_VAL = 1) generate
    begin
        process
        begin
            b <= '1';
            wait for 1 ns;
            b <= '0';
            wait for 1 ns;
            wait;
        end process;

    end generate;

end architecture;
