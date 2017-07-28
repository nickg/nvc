-- test_ng.vhd
entity  TEST_REGS is
    generic (
        REGS_BITS   : integer := 32
    );
    port (
        CLK         : in  bit;
        RST         : in  bit;
        REGS_WEN    : in  bit_vector(REGS_BITS-1 downto 0);
        REGS_WDATA  : in  bit_vector(REGS_BITS-1 downto 0);
        REGS_RDATA  : out bit_vector(REGS_BITS-1 downto 0)
    );
end TEST_REGS;
architecture RTL of TEST_REGS is
    signal curr_value : bit_vector(REGS_BITS-1 downto 0);
begin
    process (CLK, RST) begin
        if    (RST = '1') then
            curr_value <= (others => '0');
        elsif (CLK'event and CLK = '1') then
            for i in curr_value'range loop
                if (i >= REGS_WEN'low and i <= REGS_WEN'high) then
                    if (REGS_WEN(i) = '1') then
                        curr_value(i) <= REGS_WDATA(i);
                    end if;
                end if;
            end loop;
        end if;
    end process;
    REGS_RDATA(0)                    <= curr_value(0);
    REGS_RDATA(REGS_BITS-1 downto 1) <= curr_value(REGS_BITS-1 downto 1);
end RTL;

entity  TEST_NG is
end     TEST_NG;
architecture MODEL of TEST_NG is
    component TEST_REGS
        generic (
            REGS_BITS   : integer := 32
        );
        port (
            CLK         : in  bit;
            RST         : in  bit;
            REGS_WEN    : in  bit_vector(REGS_BITS-1 downto 0);
            REGS_WDATA  : in  bit_vector(REGS_BITS-1 downto 0);
            REGS_RDATA  : out bit_vector(REGS_BITS-1 downto 0)
        );
    end component;
    constant  CLK_PERIOD        : time := 10 ns;
    constant  DELAY             : time :=  2 ns;
    signal    CLK               : bit;
    signal    RST               : bit;
    constant  REGS_BITS         : integer   := 32;
    signal    regs_wen          : bit_vector(REGS_BITS-1 downto 0);
    signal    regs_wdata        : bit_vector(REGS_BITS-1 downto 0);
    signal    regs_rdata        : bit_vector(REGS_BITS-1 downto 0);
begin
    REGS: TEST_REGS                     --
        generic map (                                --
            REGS_BITS       => REGS_BITS             --
        )                                            --
        port map (                                   --
            CLK             => CLK                 , -- In  :
            RST             => RST                 , -- In  :
            REGS_WEN        => regs_wen            , -- In  :
            REGS_WDATA      => regs_wdata          , -- In  :
            REGS_RDATA      => open                  -- Out : NG if open, OK if regs_rdata
       );
    process begin
        CLK <= '1'; wait for CLK_PERIOD/2;
        CLK <= '0'; wait for CLK_PERIOD/2;
    end process;
    TEST: process
    begin
        RST        <= '1';
        regs_wen   <= (others => '0');
        regs_wdata <= (others => '0');
        wait until (CLK'event and CLK = '1');
        RST        <= '0' after DELAY;
        wait until (CLK'event and CLK = '1');
        regs_wen   <= (others => '1') after DELAY;
        regs_wdata <= (others => '0') after DELAY;
        wait until (CLK'event and CLK = '1');
        regs_wen   <= (others => '0') after DELAY;
        wait until (CLK'event and CLK = '1');
        wait;
    end process;
end MODEL;
