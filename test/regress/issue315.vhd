-- test_sync.vhd
entity  TEST_SYNC is
    generic (
        BITS : integer := 1
    );
    port (
        I    : in  bit_vector(BITS-1 downto 0);
        O    : out bit_vector(BITS-1 downto 0)
    );
end TEST_SYNC;
architecture MODEL of TEST_SYNC is
begin
    O <= I;
end MODEL;

-- test_ng.vhd
entity  TEST_NG is
    generic (
        DATA_BITS  : integer := 32
    );
    port (
        I_DATA     : in  bit_vector(DATA_BITS-1 downto 0);
        O_DATA     : out bit_vector(DATA_BITS-1 downto 0)
    );
end     TEST_NG;
architecture RTL of TEST_NG is
    component TEST_SYNC
        generic (BITS: integer := 1);
        port(
            I: in  bit_vector(BITS-1 downto 0);
            O: out bit_vector(BITS-1 downto 0)
        );
    end component;
    type      INFO_TYPE is record
              DATA_LO   : integer;
              DATA_HI   : integer;
              BITS      : integer;
    end record;
    function  SET_INFO return INFO_TYPE is
        variable info   : INFO_TYPE;
        variable index  : integer;
    begin
        index        := 0;
        info.DATA_LO := index;
        info.DATA_HI := index + DATA_BITS - 1;
        index        := index + DATA_BITS;
        info.BITS    := index;
        return info;
    end function;
    constant  INFO    : INFO_TYPE := SET_INFO;
    signal    i_info  : bit_vector(INFO.BITS-1 downto 0);
    signal    o_info  : bit_vector(INFO.BITS-1 downto 0);
begin
    i_info(INFO.DATA_HI downto INFO.DATA_LO) <= I_DATA;
    SYNC: TEST_SYNC generic map (
            BITS => INFO.BITS
        )
        port map(
            I    => i_info,
            O    => o_info
        );
    O_DATA <= o_info(INFO.DATA_HI downto INFO.DATA_LO);
end RTL;

-------------------------------------------------------------------------------

entity issue315 is
end entity;

architecture test of issue315 is
    signal i, o : bit_vector(31 downto 0);
begin

    uut: entity work.test_ng
        port map (i, o );

    process is
    begin
        i <= X"12345678";
        wait for 1 ns;
        assert o = X"12345678";
        wait;
    end process;

end architecture;
