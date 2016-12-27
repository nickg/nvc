-- a_ng.vhd
package TEST_TYPES is
    type WIDTH_TYPE is record
        DATA : integer;
    end record;
end package;

use     work.TEST_TYPES.all;
entity  TEST_SUB is
    generic (
        WIDTH  : WIDTH_TYPE
    );
    port (
        DATA_I : in  bit_vector(WIDTH.DATA-1 downto 0);
        DATA_O : out bit_vector(WIDTH.DATA-1 downto 0)
    );
end TEST_SUB;
architecture MODEL of TEST_SUB is
begin
    DATA_O <= DATA_I;
end MODEL;

use     work.TEST_TYPES.all;
entity  TEST_NG is
end TEST_NG;
architecture MODEL of TEST_NG is
    constant WIDTH  : WIDTH_TYPE := (DATA => 8);  -- Could not fold this
    signal   DATA_I : bit_vector(WIDTH.DATA-1 downto 0);
    signal   DATA_O : bit_vector(WIDTH.DATA-1 downto 0);
begin
    DUT: entity WORK.TEST_SUB
        generic map (WIDTH  => WIDTH)
        port map    (DATA_I => DATA_I, DATA_O => DATA_O);
    process begin
        DATA_I <= "00000000";
        wait for 10 ns;
        assert(DATA_O /= "00000000") report "OK." severity NOTE;
        assert(DATA_O  = "00000000") report "NG." severity ERROR;
        DATA_I <= "00000001";
        wait for 10 ns;
        assert(DATA_O /= "00000001") report "OK." severity NOTE;
        assert(DATA_O  = "00000001") report "NG." severity ERROR;
        assert FALSE report "Simulation complete." severity FAILURE;
    end process;
end MODEL;
