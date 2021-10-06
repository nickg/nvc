-----------------------------------------------------------------------------------
-- Used to crash randomly during constant folding
-----------------------------------------------------------------------------------
package SMPL_TYPES is
    constant  SMPL_ADDR_MAX_WIDTH  :  integer := 64;
    constant  SMPL_DATA_MAX_WIDTH  :  integer := 1024;
    constant  SMPL_STRB_MAX_WIDTH  :  integer := SMPL_DATA_MAX_WIDTH/8;
    constant  SMPL_SIZE_MAX_WIDTH  :  integer := 8;

    constant  SMPL_INFO_WIDTH      :  integer := 4;
    subtype   SMPL_INFO_TYPE       is bit_vector(SMPL_INFO_WIDTH-1 downto 0);

    constant  SMPL_RESP_WIDTH      :  integer := 2;
    subtype   SMPL_RESP_TYPE       is bit_vector(SMPL_RESP_WIDTH-1 downto 0);

    type      SMPL_A_SIGNALS_TYPE is record
        ADDR     : bit_vector(SMPL_ADDR_MAX_WIDTH-1 downto 0);
        WRITE    : bit;
        SIZE     : bit_vector(SMPL_SIZE_MAX_WIDTH-1 downto 0);
        INFO     : SMPL_INFO_TYPE;
        VALID    : bit;
        READY    : bit;
    end record;
    constant  SMPL_A_SIGNALS_DONTCARE : SMPL_A_SIGNALS_TYPE := (
        ADDR    => (others => '1'),
        WRITE   => '1',
        SIZE    => (others => '1'),
        INFO    => (others => '1'),
        VALID   => '1',
        READY   => '1'
    );
    constant  SMPL_A_SIGNALS_NULL     : SMPL_A_SIGNALS_TYPE := (
        ADDR    => (others => '0'),
        WRITE   => '0',
        SIZE    => (others => '0'),
        INFO    => (others => '0'),
        VALID   => '0',
        READY   => '0'
    );
    type      SMPL_W_SIGNALS_TYPE is record
        DATA     : bit_vector(SMPL_DATA_MAX_WIDTH-1 downto 0);
        STRB     : bit_vector(SMPL_STRB_MAX_WIDTH-1 downto 0);
        VALID    : bit;
        READY    : bit;
    end record;
    constant  SMPL_W_SIGNALS_DONTCARE : SMPL_W_SIGNALS_TYPE := (
        DATA    => (others => '1'),
        STRB    => (others => '1'),
        VALID   => '1',
        READY   => '1'
    );
    constant  SMPL_W_SIGNALS_NULL     : SMPL_W_SIGNALS_TYPE := (
        DATA    => (others => '0'),
        STRB    => (others => '0'),
        VALID   => '0',
        READY   => '0'
    );
    type      SMPL_R_SIGNALS_TYPE is record
        DATA     : bit_vector(SMPL_DATA_MAX_WIDTH-1 downto 0);
        RESP     : SMPL_RESP_TYPE;
        VALID    : bit;
        READY    : bit;
    end record;
    constant  SMPL_R_SIGNALS_DONTCARE : SMPL_R_SIGNALS_TYPE := (
        DATA    => (others => '1'),
        RESP    => (others => '1'),
        VALID   => '1',
        READY   => '1'
    );
    constant  SMPL_R_SIGNALS_NULL     : SMPL_R_SIGNALS_TYPE := (
        DATA    => (others => '0'),
        RESP    => (others => '0'),
        VALID   => '0',
        READY   => '0'
    );
    type      SMPL_B_SIGNALS_TYPE is record
        RESP     : SMPL_RESP_TYPE;
        VALID    : bit;
        READY    : bit;
    end record;
    constant  SMPL_B_SIGNALS_DONTCARE : SMPL_B_SIGNALS_TYPE := (
        RESP    => (others => '1'),
        VALID   => '1',
        READY   => '1'
    );
    constant  SMPL_B_SIGNALS_NULL     : SMPL_B_SIGNALS_TYPE := (
        RESP    => (others => '0'),
        VALID   => '0',
        READY   => '0'
    );
    type      SMPL_SIGNALS_TYPE is record
        AR       : SMPL_A_SIGNALS_TYPE;
        AW       : SMPL_A_SIGNALS_TYPE;
        R        : SMPL_R_SIGNALS_TYPE;
        W        : SMPL_W_SIGNALS_TYPE;
        B        : SMPL_B_SIGNALS_TYPE;
    end record;
    constant  SMPL_SIGNALS_DONTCARE : SMPL_SIGNALS_TYPE := (
        AR      => SMPL_A_SIGNALS_DONTCARE,
        AW      => SMPL_A_SIGNALS_DONTCARE,
        R       => SMPL_R_SIGNALS_DONTCARE,
        W       => SMPL_W_SIGNALS_DONTCARE,
        B       => SMPL_B_SIGNALS_DONTCARE
    );
end SMPL_TYPES;
-----------------------------------------------------------------------------------
--
-----------------------------------------------------------------------------------
use     WORK.SMPL_TYPES.all;
entity  CHANNEL_PLAYER is
    generic (
        CHANNEL         : integer;
        MASTER          : boolean   := FALSE
    );
end CHANNEL_PLAYER;
architecture MODEL of CHANNEL_PLAYER is
    function  GEN_INIT_signals return SMPL_SIGNALS_TYPE is
        variable  value : SMPL_SIGNALS_TYPE;
    begin
        value := SMPL_SIGNALS_DONTCARE;
        if (MASTER) then
            case CHANNEL is
                when 1 =>
                    value.AR       := SMPL_A_SIGNALS_NULL;
                    value.AR.READY := '1';
                when 2 =>
                    value.AW       := SMPL_A_SIGNALS_NULL;
                    value.AW.READY := '1';
                when 3  =>
                    value.W        := SMPL_W_SIGNALS_NULL;
                    value.W.READY  := '1';
                when 4  =>
                    value.R        := SMPL_R_SIGNALS_DONTCARE;
                    value.R.READY  := '0';
                when 5  =>
                    value.B        := SMPL_B_SIGNALS_DONTCARE;
                    value.B.READY  := '0';
                when others =>
                    null;
            end case;
        end if;
        return value;
    end function;
    constant  INIT_SIGNALS  : SMPL_SIGNALS_TYPE := GEN_INIT_SIGNALS;
begin
    process
        variable  out_signals   : SMPL_SIGNALS_TYPE;
    begin
        out_signals := INIT_SIGNALS;
        wait;
    end process;
end MODEL;
-----------------------------------------------------------------------------------
--
-----------------------------------------------------------------------------------
entity  MASTER_PLAYER is
end MASTER_PLAYER;
architecture MODEL of MASTER_PLAYER is
begin
    C0: entity WORK.CHANNEL_PLAYER generic map (0, FALSE);
    C1: entity WORK.CHANNEL_PLAYER generic map (1, TRUE );
    C2: entity WORK.CHANNEL_PLAYER generic map (2, TRUE );
    C3: entity WORK.CHANNEL_PLAYER generic map (3, TRUE );
    C4: entity WORK.CHANNEL_PLAYER generic map (4, TRUE );
    C5: entity WORK.CHANNEL_PLAYER generic map (5, TRUE );
end MODEL;
-----------------------------------------------------------------------------------
--
-----------------------------------------------------------------------------------
entity  issue425 is
end     issue425;
architecture MODEL of issue425 is
begin
    M: entity WORK.MASTER_PLAYER;
    S: entity WORK.MASTER_PLAYER;
end MODEL;
