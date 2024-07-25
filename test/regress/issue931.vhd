package nvc_bug_pkg is

    constant C_SLV_SIZE : integer := 16;

end package nvc_bug_pkg;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library work;
use work.nvc_bug_pkg.all;

entity issue931 is
    generic (
        GC_SLV : std_logic_vector(C_SLV_SIZE-1 downto 0) := x"8008"
    );
end entity;

architecture test of issue931 is
begin

    p_nvc_bug : process
    begin
        assert GC_SLV(C_SLV_SIZE-1 downto 0) = x"8008" report "Mismatch. Expected 8008, got " & to_hstring(GC_SLV(C_SLV_SIZE-1 downto 0)) severity failure;
        report "Test succeeded";
        wait;
    end process p_nvc_bug;

end architecture test;
