library ieee;
use     ieee.std_logic_1164.all;
package SYNC is
    constant  SYNC_MAX_PLUG_SIZE   :  integer := 4;
    subtype   SYNC_PLUG_NUM_TYPE   is integer range 1 to SYNC_MAX_PLUG_SIZE;
    subtype   SYNC_SIG_TYPE        is std_logic_vector(1 to SYNC_MAX_PLUG_SIZE);
    subtype   SYNC_REQ_TYPE        is integer;
    subtype   SYNC_ACK_TYPE        is std_logic;
    type      SYNC_REQ_VECTOR      is array (INTEGER range <>) of SYNC_REQ_TYPE;
    type      SYNC_ACK_VECTOR      is array (INTEGER range <>) of SYNC_ACK_TYPE;
    component SYNC_SIG_DRIVER
        generic (
            PLUG_NUM : SYNC_PLUG_NUM_TYPE := 1
        );
        port    (
            SYNC     : inout SYNC_SIG_TYPE := (others => 'Z');  -- Was 'U'
            REQ      : in    SYNC_REQ_TYPE;
            ACK      : out   SYNC_ACK_TYPE
        );
    end component;
end package;

library ieee;
use     ieee.std_logic_1164.all;
library WORK;
use     WORK.SYNC.all;
entity  SYNC_SIG_DRIVER_SUB_UNIT is
    port (
        SYNC_I   : in    SYNC_SIG_TYPE;
        SYNC_O   : out   std_logic;
        REQ      : in    SYNC_REQ_TYPE;
        ACK      : out   SYNC_ACK_TYPE
    );
end     SYNC_SIG_DRIVER_SUB_UNIT;
architecture MODEL of SYNC_SIG_DRIVER_SUB_UNIT is
    function ALL_ONE(SYNC : SYNC_SIG_TYPE) return boolean is
        variable sync_vec : SYNC_SIG_TYPE;
        constant all_1    : SYNC_SIG_TYPE := (others => '1');
    begin
        for i in SYNC'range loop
            if (SYNC(i) = '0') then
                sync_vec(i) := '0';
            else
                sync_vec(i) := '1';
            end if;
        end loop;
        if (sync_vec = all_1) then
            return true;
        else
            return false;
        end if;
    end function;
begin
    process begin
        SYNC_O <= 'Z';
        ACK    <= '0';
        SYNC_LOOP: loop
            if (REQ > 0) then
                SYNC_O <= 'H';
                wait until (ALL_ONE(SYNC_I));  -- This line causes an error
                SYNC_O <= '0';
                ACK    <= '1';
                wait until (REQ = 0);
                ACK    <= '0';
            elsif (REQ = 0) then
                SYNC_O <= '0';
            else
                SYNC_O <= 'Z';
            end if;
            wait on REQ;
        end loop;
    end process;
end MODEL;

library ieee;
use     ieee.std_logic_1164.all;
use     std.textio.all;
library WORK;
use     WORK.SYNC.all;
entity  SYNC_SIG_DRIVER is
    generic (
        PLUG_NUM : SYNC_PLUG_NUM_TYPE := 1
    );
    port    (
        SYNC     : inout SYNC_SIG_TYPE := (others => 'Z');  -- Was 'U'
        REQ      : in    SYNC_REQ_TYPE;
        ACK      : out   SYNC_ACK_TYPE
     );
end     SYNC_SIG_DRIVER;
architecture MODEL of SYNC_SIG_DRIVER is
    component SYNC_SIG_DRIVER_SUB_UNIT is
        port (
            SYNC_I   : in    SYNC_SIG_TYPE;
            SYNC_O   : out   std_logic;
            REQ      : in    SYNC_REQ_TYPE;
            ACK      : out   SYNC_ACK_TYPE
        );
    end component;
begin
    U: SYNC_SIG_DRIVER_SUB_UNIT
        port map(
            SYNC_I   => SYNC,
            SYNC_O   => SYNC(PLUG_NUM),
            REQ      => REQ,
            ACK      => ACK
        );
end MODEL;

library ieee;
use     ieee.std_logic_1164.all;
library WORK;
use     WORK.SYNC.all;
entity  issue428 is
end     issue428;
architecture MODEL of issue428 is
    constant  PLUG_SIZE  :  integer := 2;
    signal    SYNC       :  SYNC_SIG_TYPE;
    signal    REQ        :  SYNC_REQ_VECTOR(1 to PLUG_SIZE);
    signal    ACK        :  SYNC_ACK_VECTOR(1 to PLUG_SIZE);
begin
    PLUG : for i in 1 to PLUG_SIZE generate
        DRIVER : SYNC_SIG_DRIVER
            generic map (PLUG_NUM => i)
            port    map (SYNC => SYNC, REQ => REQ(i), ACK => ACK(i));
    end generate;
    process begin
        REQ(1) <= 0;
        wait;
    end process;
    process begin
        assert sync = "UUUU";
        wait for 0 ns;
        assert sync = "UUUU";
        wait for 5 ns;
        REQ(2) <= 1;
        wait for 10 ns;
        report std_logic'image(sync(1));
        report std_logic'image(sync(2));
        report std_logic'image(sync(3));
        report std_logic'image(sync(4));
        assert sync = "XX11";           -- Aldec has UXUU (with SYNC default of
                                        -- 'U')
        assert ack(2) = '1';
        wait;
    end process;

    sync <= (others => '1') after 10 ns;
end MODEL;
