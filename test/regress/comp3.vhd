lIBRARY IEEE;   USE IEEE.std_logic_1164.ALL;
                USE IEEE.VITAL_timing.ALL;

package pack is
    CONSTANT DefaultInstancePath : STRING  := "*";
    CONSTANT DefaultTimingChecks : BOOLEAN := FALSE;
    CONSTANT DefaultTimingModel  : STRING  := "UNIT";
    CONSTANT DefaultXon          : BOOLEAN := TRUE;
    CONSTANT DefaultMsgOn        : BOOLEAN := TRUE;

    CONSTANT UnitDelay     : VitalDelayType     := 1 ns;
    CONSTANT UnitDelay01   : VitalDelayType01   := (1 ns, 1 ns);
    CONSTANT UnitDelay01Z  : VitalDelayType01Z  := (others => 1 ns);
    CONSTANT UnitDelay01ZX : VitalDelayType01ZX := (others => 1 ns);
end package;

-------------------------------------------------------------------------------

LIBRARY IEEE;   USE IEEE.std_logic_1164.ALL;
USE IEEE.vital_timing.ALL;

use work.pack.all;
--------------------------------------------------------------------------------
-- ENTITY DECLARATION
--------------------------------------------------------------------------------
ENTITY sram1k8 IS
    GENERIC (
        -- tipd delays: interconnect path delays
        tipd_OENeg          : VitalDelayType01 := VitalZeroDelay01;
        tipd_WENeg          : VitalDelayType01 := VitalZeroDelay01;
        tipd_CENeg          : VitalDelayType01 := VitalZeroDelay01;
        tipd_CE             : VitalDelayType01 := VitalZeroDelay01;
        tipd_D0             : VitalDelayType01 := VitalZeroDelay01;
        tipd_D1             : VitalDelayType01 := VitalZeroDelay01;
        tipd_D2             : VitalDelayType01 := VitalZeroDelay01;
        tipd_D3             : VitalDelayType01 := VitalZeroDelay01;
        tipd_D4             : VitalDelayType01 := VitalZeroDelay01;
        tipd_D5             : VitalDelayType01 := VitalZeroDelay01;
        tipd_D6             : VitalDelayType01 := VitalZeroDelay01;
        tipd_D7             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A0             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A1             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A2             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A3             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A4             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A5             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A6             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A7             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A8             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A9             : VitalDelayType01 := VitalZeroDelay01;
        -- tpd delays
        tpd_OENeg_D0                    : VitalDelayType01Z := UnitDelay01Z;
        tpd_CENeg_D0                    : VitalDelayType01Z := UnitDelay01Z;
        tpd_A0_D0                       : VitalDelayType01  := UnitDelay01;
        -- tpw values: pulse widths
        tpw_WENeg_negedge               : VitalDelayType    := UnitDelay;
        tpw_WENeg_posedge               : VitalDelayType    := UnitDelay;
        -- tsetup values: setup times
        tsetup_D0_WENeg                 : VitalDelayType    := UnitDelay;
        tsetup_D0_CENeg                 : VitalDelayType    := UnitDelay;
        -- thold values: hold times
        thold_D0_WENeg                  : VitalDelayType    := UnitDelay;
        thold_D0_CENeg                  : VitalDelayType    := UnitDelay;
        -- generic control parameters
        InstancePath        : STRING    := DefaultInstancePath;
        TimingChecksOn      : BOOLEAN   := DefaultTimingChecks;
        MsgOn               : BOOLEAN   := DefaultMsgOn;
        XOn                 : BOOLEAN   := DefaultXOn;
        SeverityMode        : SEVERITY_LEVEL := WARNING;
        -- For FMF SDF technology file usage
        TimingModel         : STRING    := DefaultTimingModel
    );
    PORT (
        A0              : IN    std_logic := 'X';
        A1              : IN    std_logic := 'X';
        A2              : IN    std_logic := 'X';
        A3              : IN    std_logic := 'X';
        A4              : IN    std_logic := 'X';
        A5              : IN    std_logic := 'X';
        A6              : IN    std_logic := 'X';
        A7              : IN    std_logic := 'X';
        A8              : IN    std_logic := 'X';
        A9              : IN    std_logic := 'X';

        D0              : INOUT std_logic := 'X';
        D1              : INOUT std_logic := 'X';
        D2              : INOUT std_logic := 'X';
        D3              : INOUT std_logic := 'X';
        D4              : INOUT std_logic := 'X';
        D5              : INOUT std_logic := 'X';
        D6              : INOUT std_logic := 'X';
        D7              : INOUT std_logic := 'X';

        OENeg           : IN    std_logic := 'X';
        WENeg           : IN    std_logic := 'X';
        CENeg           : IN    std_logic := 'X';
        CE              : IN    std_logic := 'X'
    );
    ATTRIBUTE VITAL_LEVEL0 of sram1k8 : ENTITY IS TRUE;
END sram1k8;

--------------------------------------------------------------------------------
-- ARCHITECTURE DECLARATION
--------------------------------------------------------------------------------
ARCHITECTURE vhdl_behavioral of sram1k8 IS
BEGIN
    check: process is
    begin
        wait for 1 ns;
        assert instancepath = "*";
        assert tipd_D7 = VitalZeroDelay01;
        wait;
    end process;

END vhdl_behavioral;

-------------------------------------------------------------------------------

LIBRARY IEEE;   USE IEEE.std_logic_1164.ALL;
                USE IEEE.VITAL_timing.ALL;

use work.pack.all;

ENTITY comp3 IS END;

ARCHITECTURE test_1 of comp3 IS

    COMPONENT sram1k8
    GENERIC (
        -- tipd delays: interconnect path delays
        tipd_OENeg          : VitalDelayType01 := VitalZeroDelay01;
        tipd_WENeg          : VitalDelayType01 := VitalZeroDelay01;
        tipd_CENeg          : VitalDelayType01 := (2 ns, 2 ns);
        tipd_CE             : VitalDelayType01 := VitalZeroDelay01;
        tipd_D0             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A0             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A1             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A2             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A3             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A4             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A5             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A6             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A7             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A8             : VitalDelayType01 := VitalZeroDelay01;
        tipd_A9             : VitalDelayType01 := VitalZeroDelay01;
        -- tpd delays
        tpd_OENeg_D0                    : VitalDelayType01Z := UnitDelay01Z;
        tpd_CENeg_D0                    : VitalDelayType01Z := UnitDelay01Z;
        tpd_A0_D0                       : VitalDelayType01  := UnitDelay01;
        -- tpw values: pulse widths
        tpw_WENeg_negedge               : VitalDelayType    := UnitDelay;
        tpw_WENeg_posedge               : VitalDelayType    := UnitDelay;
        -- tsetup values: setup times
        tsetup_D0_WENeg                 : VitalDelayType    := UnitDelay;
        tsetup_D0_CENeg                 : VitalDelayType    := UnitDelay;
        -- thold values: hold times
        thold_D0_WENeg                  : VitalDelayType    := UnitDelay;
        thold_D0_CENeg                  : VitalDelayType    := UnitDelay;
        -- generic control parameters
        InstancePath        : STRING    := DefaultInstancePath;
        TimingChecksOn      : BOOLEAN   := DefaultTimingChecks;
        MsgOn               : BOOLEAN   := DefaultMsgOn;
        XOn                 : BOOLEAN   := DefaultXOn;
        SeverityMode        : SEVERITY_LEVEL := WARNING;
        -- For FMF SDF technology file usage
        TimingModel         : STRING    := DefaultTimingModel
    );
    PORT (
        A0              : IN    std_logic := 'X';
        A1              : IN    std_logic := 'X';
        A2              : IN    std_logic := 'X';
        A3              : IN    std_logic := 'X';
        A4              : IN    std_logic := 'X';
        A5              : IN    std_logic := 'X';
        A6              : IN    std_logic := 'X';
        A7              : IN    std_logic := 'X';
        A8              : IN    std_logic := 'X';
        A9              : IN    std_logic := 'X';

        D0              : INOUT std_logic := 'X';
        D1              : INOUT std_logic := 'X';
        D2              : INOUT std_logic := 'X';
        D3              : INOUT std_logic := 'X';
        D4              : INOUT std_logic := 'X';
        D5              : INOUT std_logic := 'X';
        D6              : INOUT std_logic := 'X';
        D7              : INOUT std_logic := 'X';

        OENeg           : IN    std_logic := 'X';
        WENeg           : IN    std_logic := 'X';
        CENeg           : IN    std_logic := 'X';
        CE              : IN    std_logic := 'X'
    );
    END COMPONENT;

    for all : sram1k8 use entity WORK.sram1k8(VHDL_BEHAVIORAL);

    SIGNAL T_CLKIN           : std_logic := 'X';
    SIGNAL T_CLKOUT2         : std_logic := 'X';
    SIGNAL T_CLKOUT3         : std_logic := 'X';
    SIGNAL T_CLKMODE0        : std_logic := 'X';
    SIGNAL T_TMS             : std_logic := 'X';
    SIGNAL T_TDO             : std_logic := 'X';
    SIGNAL T_TDI             : std_logic := 'X';
    SIGNAL T_TCK             : std_logic := 'X';
    SIGNAL T_TRSTNeg         : std_logic := 'X';
    SIGNAL T_EMU5            : std_logic := 'X';
    SIGNAL T_EMU4            : std_logic := 'X';
    SIGNAL T_EMU3            : std_logic := 'X';
    SIGNAL T_EMU2            : std_logic := 'X';
    SIGNAL T_HCNTL1          : std_logic := 'H';
    SIGNAL T_HCNTL0          : std_logic := 'H';
    SIGNAL T_HHWIL           : std_logic := 'Z';
    SIGNAL T_HR              : std_logic := 'Z';
    SIGNAL T_HASNeg          : std_logic := 'H';
    SIGNAL T_HCSNeg          : std_logic := 'H';
    SIGNAL T_HDS1Neg         : std_logic := 'H';
    SIGNAL T_HDS2Neg         : std_logic := 'H';
    SIGNAL T_HRDYNeg         : std_logic := 'Z';
    SIGNAL T_HD              : std_logic_vector(15 downto 0) := (others => 'Z');
    SIGNAL T_CE0Neg         : std_logic := 'X';
    SIGNAL T_BE0Neg         : std_logic := 'X';
    SIGNAL T_BE1Neg         : std_logic := 'X';
    SIGNAL T_HOLDANeg       : std_logic := 'X';
    SIGNAL T_HOLDNeg        : std_logic := 'X';
    SIGNAL T_BUSREQ         : std_logic := 'X';
    SIGNAL T_ECLKIN         : std_logic := 'X';
    SIGNAL T_ECLKOUT        : std_logic := 'X';
    SIGNAL T_SDCASNeg       : std_logic := 'X';
    SIGNAL T_SDRASNeg       : std_logic := 'X';
    SIGNAL T_SDWENeg        : std_logic := 'X';
    SIGNAL T_ARDY           : std_logic := '1';
    SIGNAL T_EA             : std_logic_vector(21 downto 2) := (others => 'Z');
    SIGNAL T_ED             : std_logic_vector(31 downto 0) := (others => 'Z');
    SIGNAL T_ROM8           : std_logic := '0';

BEGIN
    sram_1 : sram1k8
        PORT MAP(
        A0        => T_EA(2),
        A1        => T_EA(3),
        A2        => T_EA(4),
        A3        => T_EA(5),
        A4        => T_EA(6),
        A5        => T_EA(7),
        A6        => T_EA(8),
        A7        => T_EA(9),
        A8        => T_EA(10),
        A9        => T_EA(11),
        D0        => T_ED(0),
        D1        => T_ED(1),
        D2        => T_ED(2),
        D3        => T_ED(3),
        D4        => T_ED(4),
        D5        => T_ED(5),
        D6        => T_ED(6),
        D7        => T_ED(7),
        OENeg     => T_SDRASNeg,
        WENeg     => T_SDWENeg,
        CENeg     => T_CE0Neg,
        CE        => T_ROM8
        );

END test_1;
