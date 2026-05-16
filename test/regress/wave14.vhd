library ieee;
use ieee.std_logic_1164.all;

entity wave14 is
    generic (
        G_A : integer;
        G_B : integer
    );
end entity;

architecture test of wave14 is

    constant C_IN_ARCH_REAL_POSITIVE    : real := 0.314 * 10.0;
    constant C_IN_ARCH_REAL_NEGATIVE    : real := -2987.256;

    constant C_IN_ARCH_INT              : integer := integer'high;

    constant C_IN_ARCH_STD_LOGIC_1      : std_logic := '1';
    constant C_IN_ARCH_STD_LOGIC_Z      : std_logic := 'Z';
    constant C_IN_ARCH_STD_LOGIC_X      : std_logic := 'X';
    constant C_IN_ARCH_STD_LOGIC_U      : std_logic := 'U';
    constant C_IN_ARCH_STD_LOGIC_L      : std_logic := 'L';
    constant C_IN_ARCH_STD_LOGIC_H      : std_logic := 'H';
    constant C_IN_ARCH_STD_LOGIC_DC     : std_logic := '-';

    constant C_IN_ARCH_GEN_DEPENDS      : integer := G_A;

    function sum(A : integer; B : integer) return integer is
    begin
        return A + B;
    end function;

    constant C_IN_ARCH_RT_DEPENDS       : integer := sum(G_A, G_B);

    type t_enum is (
        IDLE,
        RUNNING,
        LONG_ENUM_NAME
    );

    constant C_IN_ARCH_ENUM             : t_enum := LONG_ENUM_NAME;
    constant C_IN_ARCH_PHYSICAL         : time := 45 ns * 10;
    constant C_IN_ARCH_STD_LOGIC_VECT_1 : std_logic_vector(31 downto 0) := x"DEADBEEF";

begin

    process
        constant C_IN_PROC_REAL         : real := 2.789;
        constant C_IN_PROC_INT          : integer := integer'low;
        constant C_IN_PROC_RT_EPENDS    : integer := 13 + sum(G_A, G_B);
    begin
        wait for 1 ns;
        wait;
    end process;

end architecture;
