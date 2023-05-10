package test_module_pkg is
    type entry is record
        field : integer;
    end record;
    type array_of_entries is array (0 to 5) of entry;
    type array_of_array is array (natural range<>) of array_of_entries;
end package;

-------------------------------------------------------------------------------

use work.test_module_pkg.all;

entity test_module is
    port (
        input : out array_of_array(7 downto 0)
    );
end entity test_module;

architecture rtl of test_module is

begin
    fill_array : for i in 0 to 7 generate
        fill_record : for j in 0 to 5 generate
            input(i)(j).field <= j;
        end generate;
    end generate;
end architecture;

-------------------------------------------------------------------------------

use work.test_module_pkg.all;

entity issue690 is
end entity issue690;

architecture rtl of issue690 is
    signal test_array : array_of_array(7 downto 0);
begin
    dut : entity work.test_module
    port map(
        input => test_array
    );

    test_process : process
    begin
        wait for 1 ns;
        for i in 0 to 7 loop
            for j in 0 to 5 loop
                assert test_array(i)(j).field = j
                    report "array wasn't filled properly"
                    severity failure;
            end loop;
        end loop;
        wait;
    end process;
end architecture;
