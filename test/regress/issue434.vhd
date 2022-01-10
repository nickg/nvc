-- test_ng.vhd
library ieee;
use     ieee.std_logic_1164.all;
entity  SAMPLE is
    generic (
        WORD_BITS   : integer := 8;
        STRB_BITS   : integer := 1;
        O_WIDTH     : integer := 1;
        QUEUE_SIZE  : integer := 3
    );
    port (
        CLK         : in  std_logic;
        RST         : in  std_logic;
        O_DATA      : out std_logic_vector(O_WIDTH*WORD_BITS-1 downto 0);
        O_STRB      : out std_logic_vector(O_WIDTH*STRB_BITS-1 downto 0);
        O_VAL       : out std_logic;
        O_RDY       : in  std_logic
    );
end entity;
architecture RTL of SAMPLE is
    type      WORD_TYPE    is record
              DATA         :  std_logic_vector(WORD_BITS-1 downto 0);
              STRB         :  std_logic_vector(STRB_BITS-1 downto 0);
              VAL          :  boolean;
    end record;
    constant  WORD_NULL    :  WORD_TYPE := (DATA => (others => '0'),
                                            STRB => (others => '0'),
                                            VAL  => FALSE);
    type      WORD_VECTOR  is array (INTEGER range <>) of WORD_TYPE;
    signal    curr_queue   :  WORD_VECTOR(0 to QUEUE_SIZE-1);
begin
    curr_queue <= (others => WORD_NULL);
    process (curr_queue) begin
        for i in 0 to O_WIDTH-1 loop
            O_DATA((i+1)*WORD_BITS-1 downto i*WORD_BITS) <= curr_queue(i).DATA;
            O_STRB((i+1)*STRB_BITS-1 downto i*STRB_BITS) <= curr_queue(i).STRB;
        end loop;
    end process;
end RTL;

library ieee;
use     ieee.std_logic_1164.all;
entity  issue434 is
end entity;
architecture MODEL of issue434 is
    constant   PERIOD         :  time    := 10 ns;
    constant   DELAY          :  time    :=  1 ns;
    constant   WORD_BITS      :  integer := 8;
    constant   STRB_BITS      :  integer := WORD_BITS/8;
    constant   O_WIDTH        :  integer := 2;
    constant   QUEUE_SIZE     :  integer := 4;
    signal     CLK_ENA        :  std_logic;
    signal     CLK            :  std_logic;
    signal     RST            :  std_logic;
    signal     O_DATA         :  std_logic_vector(O_WIDTH*WORD_BITS-1 downto 0);
    signal     O_STRB         :  std_logic_vector(O_WIDTH*STRB_BITS-1 downto 0);
    signal     O_VAL          :  std_logic;
    signal     O_RDY          :  std_logic;
begin
    DUT: entity WORK.SAMPLE
        generic map (
            WORD_BITS   => WORD_BITS   ,
            STRB_BITS   => STRB_BITS   ,
            O_WIDTH     => O_WIDTH     ,
            QUEUE_SIZE  => QUEUE_SIZE
        )
        port map(
            CLK         => CLK         ,
            RST         => RST         ,
            O_DATA      => O_DATA      ,
            O_STRB      => O_STRB      ,
            O_VAL       => O_VAL       ,
            O_RDY       => O_RDY
        );
    process begin
        loop
            CLK <= '1'; wait for PERIOD/2;
            CLK <= '0'; wait for PERIOD/2;
            exit when(CLK_ENA = '0');
        end loop;
        CLK <= '0';
        wait;
    end process;
    process
        procedure WAIT_CLK(CNT:integer) is
        begin
            if (CNT > 0) then
                for i in 1 to CNT loop
                    wait until (CLK'event and CLK = '1');
                end loop;
            end if;
            wait for DELAY;
        end WAIT_CLK;
    begin
        CLK_ENA <= '1';
        RST     <= '1';
        O_RDY   <= '0';
        WAIT_CLK(1);
        RST     <= '0';
        WAIT_CLK(10);
        CLK_ENA <= '0';
        assert o_data = X"0000";
        wait;
    end process;
end MODEL;
