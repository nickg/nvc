package my_package is

type slv_1_t is array (natural range <>) of bit_vector;
type slv_2_t is array (natural range <>,natural range <>) of bit_vector;
subtype my_mat_t is slv_2_t(0 to 4-1,0 to 4-1)(8-1 downto 0);

function my_function_f(arg0 : my_mat_t; constant arg2 : natural range 0 to 4-1) return slv_2_t;

end package my_package;

package body my_package is

function my_function_f(arg0 : my_mat_t; constant arg2 : natural range 0 to 4-1) return slv_2_t is
variable retval : arg0'subtype;
variable my_var_v : slv_1_t(0 to 4-1)(8-1+3 downto 0);
begin
L1: for indI in retval'range(1) loop
    L2: for indJ in retval'range(2) loop
        case indJ=arg2 is
        when false =>	null;
        when true =>
            L3: for indK in 8 to 10 loop
                -- Crash here due to control flow in case expression
                case my_var_v(indI)(indK downto indK) is
                    when B"0" => report "zero";
                    when B"1" => report "one";
                    when others => NULL;
                end case;
            end loop;
            when others => NULL;
        end case;
    end loop;
end loop;
return retval;
end function my_function_f;

end package body;

-------------------------------------------------------------------------------

entity case14 is
end entity;

use work.my_package.all;

architecture test of case14 is
begin

    p1: process is
        variable v : my_mat_t;
    begin
        wait for 1 ns;
        assert my_function_f(v, 0) = v;
        wait;
    end process;

end architecture;
