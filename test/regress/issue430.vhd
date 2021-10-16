library ieee;
use     ieee.std_logic_1164.all;
package TYPES is
    constant  ADDR_MAX_WIDTH       : integer := 64;
    constant  DATA_MAX_WIDTH       : integer := 1024;

    type      CHANNEL_TYPE is (
        CHANNEL_AR,
        CHANNEL_AW,
        CHANNEL_DR,
        CHANNEL_DW,
        CHANNEL_M
    );

    type      ADDR_CHANNEL_SIGNAL_TYPE is record
        ADDR     : std_logic_vector(ADDR_MAX_WIDTH -1 downto 0);
        WRITE    : std_logic;
        VALID    : std_logic;
        READY    : std_logic;
    end record;
    type      DATA_CHANNEL_SIGNAL_TYPE is record
        DATA     : std_logic_vector(DATA_MAX_WIDTH-1 downto 0);
        LAST     : std_logic;
        VALID    : std_logic;
        READY    : std_logic;
    end record;
    type      CHANNEL_SIGNAL_TYPE is record
        AR       : ADDR_CHANNEL_SIGNAL_TYPE;
        DR       : DATA_CHANNEL_SIGNAL_TYPE;
        AW       : ADDR_CHANNEL_SIGNAL_TYPE;
        DW       : DATA_CHANNEL_SIGNAL_TYPE;
    end record;
end package;

-----------------------------------------------------------------------------------
--! @brief   CHANNEL_PLAYER :
-----------------------------------------------------------------------------------
library ieee;
use     ieee.std_logic_1164.all;
library WORK;
use     WORK.TYPES.all;
entity  CHANNEL_PLAYER is
    generic (
        CHANNEL         : CHANNEL_TYPE;
        MASTER          : boolean   := FALSE;
        SLAVE           : boolean   := FALSE;
        READ_ENABLE     : boolean   := TRUE;
        WRITE_ENABLE    : boolean   := TRUE;
        ADDR_WIDTH      : integer   := 32;
        DATA_WIDTH      : integer   := 32
    );
    port(
        ACLK            : in    std_logic;
        ARESETn         : in    std_logic;
        ARADDR_I        : in    std_logic_vector(ADDR_WIDTH-1 downto 0);
        ARADDR_O        : out   std_logic_vector(ADDR_WIDTH-1 downto 0);
        ARVALID_I       : in    std_logic;
        ARVALID_O       : out   std_logic;
        ARREADY_I       : in    std_logic;
        ARREADY_O       : out   std_logic;
        RVALID_I        : in    std_logic;
        RVALID_O        : out   std_logic;
        RLAST_I         : in    std_logic;
        RLAST_O         : out   std_logic;
        RDATA_I         : in    std_logic_vector(DATA_WIDTH-1 downto 0);
        RDATA_O         : out   std_logic_vector(DATA_WIDTH-1 downto 0);
        RREADY_I        : in    std_logic;
        RREADY_O        : out   std_logic;
        AWADDR_I        : in    std_logic_vector(ADDR_WIDTH-1 downto 0);
        AWADDR_O        : out   std_logic_vector(ADDR_WIDTH-1 downto 0);
        AWVALID_I       : in    std_logic;
        AWVALID_O       : out   std_logic;
        AWREADY_I       : in    std_logic;
        AWREADY_O       : out   std_logic;
        WLAST_I         : in    std_logic;
        WLAST_O         : out   std_logic;
        WDATA_I         : in    std_logic_vector(DATA_WIDTH-1 downto 0);
        WDATA_O         : out   std_logic_vector(DATA_WIDTH-1 downto 0);
        WVALID_I        : in    std_logic;
        WVALID_O        : out   std_logic;
        WREADY_I        : in    std_logic;
        WREADY_O        : out   std_logic;
        FINISH          : out   std_logic
    );
end CHANNEL_PLAYER;
architecture MODEL of CHANNEL_PLAYER is
    procedure function_that_dies_with_this(signals : inout CHANNEL_SIGNAL_TYPE) is
        procedure read_val(val: out std_logic_vector) is
            constant null_val : std_logic_vector(val'length-1 downto 0) := (others => '0');
        begin
            val := null_val;
        end procedure;
    begin
        read_val(signals.AR.ADDR(ADDR_WIDTH-1 downto 0));
    end procedure;
begin
    CHANNEL_M: if (CHANNEL = CHANNEL_M) generate
        PROCESS_M: process
        begin
            FINISH <= '1';
            wait;
        end process;
    end generate;
    CHANNEL_A:if (CHANNEL = CHANNEL_AW or CHANNEL = CHANNEL_AR) generate
        PROCESS_A: process
            procedure execute_output is
            begin
                if (MASTER and WRITE_ENABLE and CHANNEL = CHANNEL_AW) then
                    AWADDR_O  <= (others => '0');
                    AWVALID_O <= '0';
                end if;
                if (MASTER and READ_ENABLE  and CHANNEL = CHANNEL_AR) then
                    ARADDR_O  <= (others => '0');
                    ARVALID_O <= '0';
                end if;
                if (SLAVE  and WRITE_ENABLE and CHANNEL = CHANNEL_AW) then
                    AWREADY_O <= '0';
                end if;
                if (SLAVE  and READ_ENABLE  and CHANNEL = CHANNEL_AR) then
                    ARREADY_O <= '0';
                end if;
            end procedure;
        begin
            FINISH         <= '1';
            wait;
        end process;
    end generate;
    CHANNEL_D:if (CHANNEL = CHANNEL_DW  or CHANNEL = CHANNEL_DR) generate
        PROCESS_D: process
            procedure execute_output is
            begin
                if (MASTER and WRITE_ENABLE and CHANNEL = CHANNEL_DW) then
                    WDATA_O   <= (others => '0');
                    WLAST_O   <= '0';
                    WVALID_O  <= '0';
                end if;
                if (MASTER and READ_ENABLE  and CHANNEL = CHANNEL_DR) then
                    RREADY_O  <= '0';
                end if;
                if (SLAVE  and WRITE_ENABLE and CHANNEL = CHANNEL_DW) then
                    WREADY_O  <= '0';
                end if;
                if (SLAVE  and READ_ENABLE  and CHANNEL = CHANNEL_DR) then
                    RDATA_O   <= (others => '0');
                    RLAST_O   <= '0';
                    RVALID_O  <= '0';
                end if;
            end procedure;
        begin
            execute_output;             -- ! add this line.
            FINISH         <= '1';
            wait;
        end process;
    end generate;
end MODEL;

-----------------------------------------------------------------------------------
--
-----------------------------------------------------------------------------------
library ieee;
use     ieee.std_logic_1164.all;
library WORK;
use     WORK.TYPES.all;
entity  ISSUE430 is
end     ISSUE430;
architecture MODEL of ISSUE430 is
    constant  READ_ENABLE     : boolean   := TRUE;
    constant  WRITE_ENABLE    : boolean   := TRUE;
    constant  ADDR_WIDTH      : integer := 32;
    constant  DATA_WIDTH      : integer := 32;
    signal    ACLK            : std_logic;
    signal    ARESETn         : std_logic;
    signal    ARADDR          : std_logic_vector(ADDR_WIDTH-1 downto 0);
    signal    ARVALID         : std_logic;
    signal    ARREADY         : std_logic;
    signal    RLAST           : std_logic;
    signal    RDATA           : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal    RVALID          : std_logic;
    signal    RREADY          : std_logic;
    signal    AWADDR          : std_logic_vector(ADDR_WIDTH-1 downto 0);
    signal    AWVALID         : std_logic;
    signal    AWREADY         : std_logic;
    signal    WLAST           : std_logic;
    signal    WDATA           : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal    WVALID          : std_logic;
    signal    WREADY          : std_logic;
    signal    FINISH          : std_logic;
begin
    M: entity WORK.CHANNEL_PLAYER
        generic map (
            CHANNEL             => CHANNEL_M        ,
            MASTER              => FALSE            ,
            SLAVE               => FALSE            ,
            READ_ENABLE         => READ_ENABLE      ,
            WRITE_ENABLE        => WRITE_ENABLE     ,
            ADDR_WIDTH          => ADDR_WIDTH       ,
            DATA_WIDTH          => DATA_WIDTH
        )
        port map(
            ACLK                => ACLK             ,
            ARESETn             => ARESETn          ,
            ARADDR_I            => ARADDR           ,
            ARVALID_I           => ARVALID          ,
            ARREADY_I           => ARREADY          ,
            RVALID_I            => RVALID           ,
            RLAST_I             => RLAST            ,
            RDATA_I             => RDATA            ,
            RREADY_I            => RREADY           ,
            AWADDR_I            => AWADDR           ,
            AWVALID_I           => AWVALID          ,
            AWREADY_I           => AWREADY          ,
            WVALID_I            => WVALID           ,
            WLAST_I             => WLAST            ,
            WDATA_I             => WDATA            ,
            WREADY_I            => WREADY           ,
            FINISH              => FINISH
        );
    AR: entity WORK.CHANNEL_PLAYER
        generic map (
            CHANNEL             => CHANNEL_AR       ,
            MASTER              => TRUE             ,
            SLAVE               => FALSE            ,
            READ_ENABLE         => READ_ENABLE      ,
            WRITE_ENABLE        => WRITE_ENABLE     ,
            ADDR_WIDTH          => ADDR_WIDTH       ,
            DATA_WIDTH          => DATA_WIDTH
        )
        port map(
            ACLK                => ACLK             ,
            ARESETn             => ARESETn          ,
            ARADDR_I            => ARADDR           ,
            ARADDR_O            => ARADDR           ,
            ARVALID_I           => ARVALID          ,
            ARVALID_O           => ARVALID          ,
            ARREADY_I           => ARREADY          ,
            RVALID_I            => RVALID           ,
            RLAST_I             => RLAST            ,
            RDATA_I             => RDATA            ,
            RREADY_I            => RREADY           ,
            AWADDR_I            => AWADDR           ,
            AWVALID_I           => AWVALID          ,
            AWREADY_I           => AWREADY          ,
            WVALID_I            => WVALID           ,
            WLAST_I             => WLAST            ,
            WDATA_I             => WDATA            ,
            WREADY_I            => WREADY
        );
    DR: entity WORK.CHANNEL_PLAYER
        generic map (
            CHANNEL             => CHANNEL_DR       ,
            MASTER              => TRUE             ,
            SLAVE               => FALSE            ,
            READ_ENABLE         => READ_ENABLE      ,
            WRITE_ENABLE        => WRITE_ENABLE     ,
            ADDR_WIDTH          => ADDR_WIDTH       ,
            DATA_WIDTH          => DATA_WIDTH
        )
        port map(
            ACLK                => ACLK             ,
            ARESETn             => ARESETn          ,
            ARADDR_I            => ARADDR           ,
            ARVALID_I           => ARVALID          ,
            ARREADY_I           => ARREADY          ,
            RVALID_I            => RVALID           ,
            RLAST_I             => RLAST            ,
            RDATA_I             => RDATA            ,
            RREADY_I            => RREADY           ,
            RREADY_O            => RREADY           ,
            AWADDR_I            => AWADDR           ,
            AWVALID_I           => AWVALID          ,
            AWREADY_I           => AWREADY          ,
            WVALID_I            => WVALID           ,
            WLAST_I             => WLAST            ,
            WDATA_I             => WDATA            ,
            WREADY_I            => WREADY
        );
    AW: entity WORK.CHANNEL_PLAYER
        generic map (
            CHANNEL             => CHANNEL_AW       ,
            MASTER              => TRUE             ,
            SLAVE               => FALSE            ,
            READ_ENABLE         => READ_ENABLE      ,
            WRITE_ENABLE        => WRITE_ENABLE     ,
            ADDR_WIDTH          => ADDR_WIDTH       ,
            DATA_WIDTH          => DATA_WIDTH
        )
        port map(
            ACLK                => ACLK             ,
            ARESETn             => ARESETn          ,
            ARADDR_I            => ARADDR           ,
            ARVALID_I           => ARVALID          ,
            ARREADY_I           => ARREADY          ,
            RVALID_I            => RVALID           ,
            RLAST_I             => RLAST            ,
            RDATA_I             => RDATA            ,
            RREADY_I            => RREADY           ,
            AWADDR_I            => AWADDR           ,
            AWADDR_O            => AWADDR           ,
            AWVALID_I           => AWVALID          ,
            AWVALID_O           => AWVALID          ,
            AWREADY_I           => AWREADY          ,
            WVALID_I            => WVALID           ,
            WLAST_I             => WLAST            ,
            WDATA_I             => WDATA            ,
            WREADY_I            => WREADY
        );
    DW: entity WORK.CHANNEL_PLAYER
        generic map (
            CHANNEL             => CHANNEL_DW       ,
            MASTER              => TRUE             ,
            SLAVE               => FALSE            ,
            READ_ENABLE         => READ_ENABLE      ,
            WRITE_ENABLE        => WRITE_ENABLE     ,
            ADDR_WIDTH          => ADDR_WIDTH       ,
            DATA_WIDTH          => DATA_WIDTH
        )
        port map(
            ACLK                => ACLK             ,
            ARESETn             => ARESETn          ,
            ARADDR_I            => ARADDR           ,
            ARVALID_I           => ARVALID          ,
            ARREADY_I           => ARREADY          ,
            RVALID_I            => RVALID           ,
            RLAST_I             => RLAST            ,
            RDATA_I             => RDATA            ,
            RREADY_I            => RREADY           ,
            AWADDR_I            => AWADDR           ,
            AWVALID_I           => AWVALID          ,
            AWREADY_I           => AWREADY          ,
            WVALID_I            => WVALID           ,
            WVALID_O            => WVALID           ,
            WLAST_I             => WLAST            ,
            WLAST_O             => WLAST            ,
            WDATA_I             => WDATA            ,
            WDATA_O             => WDATA            ,
            WREADY_I            => WREADY
            );


    process is
    begin
        wait until finish = '1' for 100 ns;
        assert finish = '1';
        assert wdata = (wdata'range => '0');
        assert rvalid = 'U';
        assert wvalid = '0';
        wait;
    end process;
end MODEL;
