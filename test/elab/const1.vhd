package pack is
    function log2(x : in integer) return integer;
end package;

package body pack is
    function log2(x : in integer) return integer is
        variable r : integer := 0;
        variable c : integer := 1;
    begin
        if x <= 1 then
            r := 1;
        else
            while c < x loop
                r := r + 1;
                c := c * 2;
            end loop;
        end if;
        return r;
    end function;
end package body;

-------------------------------------------------------------------------------

entity pwm is
    generic (
        CLK_FREQ : real;
        PWM_FREQ : real );
end entity;

use work.pack.all;

architecture rtl of pwm is
    constant DIVIDE : integer := integer(CLK_FREQ / PWM_FREQ);
    constant BITS   : integer := log2(DIVIDE);

    signal ctr_r    : bit_vector(BITS - 1 downto 0) := (others => '0');
begin
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
begin
    pwm_1: entity work.pwm
        generic map (
            CLK_FREQ   => 24.0e6,
            PWM_FREQ   => 1.0e3 );
end architecture;
