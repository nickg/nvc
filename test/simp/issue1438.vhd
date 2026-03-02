package test_pkg is

    constant NUM_ENTRIES : integer := 4096;
    subtype MEM_ADDR_RANGE is integer range 0 to NUM_ENTRIES - 1;

end test_pkg;

use work.test_pkg.all;

entity test is
    port (
        clk   : in    bit;
        reset : in    bit;
        addr : in integer;
        data : out bit_vector(31 downto 0)
    );
end entity;

architecture rtl of test is

begin

    read: process(clk, reset)
    begin
        if reset then
            data <= (others => '0');
        elsif rising_edge(clk) then
            case addr is
                -- This should copy the range when simplifying
                when MEM_ADDR_RANGE =>
                    data <= (others => '1');
                when others =>
                    data <= (others => '0');
            end case;
        end if;
    end process;

end rtl;
