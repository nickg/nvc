package SYNC is
    type std_ulogic is ('0', '1', 'Z', 'H', 'U');
    type std_ulogic_vector is array (natural range <>) of std_ulogic;

    function resolved (x : in std_ulogic_vector) return std_ulogic;

    subtype std_logic is resolved std_ulogic;
    type std_logic_vector is array (natural range <>) of std_logic;

    constant  SYNC_MAX_PLUG_SIZE   :  integer := 32;
    subtype   SYNC_PLUG_NUM_TYPE   is integer range 1 to SYNC_MAX_PLUG_SIZE;
    alias     SYNC_SIG_TYPE        is std_logic;
    subtype   SYNC_REQ_TYPE        is integer;
    subtype   SYNC_ACK_TYPE        is std_logic;
    type      SYNC_REQ_VECTOR      is array (INTEGER range <>) of SYNC_REQ_TYPE;
    type      SYNC_ACK_VECTOR      is array (INTEGER range <>) of SYNC_ACK_TYPE;
    component SYNC_SIG_DRIVER
        generic (
            PLUG_NUM : SYNC_PLUG_NUM_TYPE := 1
        );
        port    (
            SYNC     : inout SYNC_SIG_TYPE;
            REQ      : in    SYNC_REQ_TYPE;
            ACK      : out   SYNC_ACK_TYPE
        );
    end component;
end package;

package body sync is

    function resolved (x : in std_ulogic_vector) return std_ulogic is
    begin
        return x(x'left);
    end function;

end package body;

library WORK;
use     WORK.SYNC.all;
entity  SYNC_SIG_DRIVER is
    generic (
        PLUG_NUM : SYNC_PLUG_NUM_TYPE := 1
    );
    port    (
        SYNC     : inout SYNC_SIG_TYPE;
        REQ      : in    SYNC_REQ_TYPE;
        ACK      : out   SYNC_ACK_TYPE
     );
end     SYNC_SIG_DRIVER;
architecture MODEL of SYNC_SIG_DRIVER is
begin
    process begin
        SYNC   <= 'Z';
        ACK    <= '0';
        SYNC_LOOP: loop
            if (REQ > 0) then
                SYNC   <= 'H';
                wait until (SYNC = '1' or SYNC = '0');
                SYNC   <= '0';
                ACK    <= '1';
                wait until (REQ = 0);
                ACK    <= '0';
            elsif (REQ = 0) then
                SYNC   <= '0';
            else
                SYNC   <= 'Z';
            end if;
            wait on REQ;
        end loop;
    end process;
end MODEL;

library WORK;
use     WORK.SYNC.all;
entity  TEST_NG is
end     TEST_NG;
architecture MODEL of TEST_NG is
    constant  PLUG_SIZE  :  integer := 2;
    signal    SYNC       :  SYNC_SIG_TYPE;
    signal    REQ        :  SYNC_REQ_VECTOR(1 to PLUG_SIZE);
    signal    ACK        :  SYNC_ACK_VECTOR(1 to PLUG_SIZE);
begin
    PLUG : for i in 1 to PLUG_SIZE generate
        DRIVER : SYNC_SIG_DRIVER
            generic map (PLUG_NUM => i)
            port    map (SYNC => SYNC,REQ => REQ(i),ACK => ACK(i));
    end generate;
    process begin
        REQ(1) <= 0;
        wait;
    end process;
    process begin
        REQ(2) <= 0;
        wait;
    end process;
end MODEL;
