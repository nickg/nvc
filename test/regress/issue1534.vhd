library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity issue1534 is
end entity;

architecture rtl of issue1534 is

    type t_sulv_array is array(natural range <>) of std_ulogic_vector;
    subtype t_byte_array is t_sulv_array(open)(7 downto 0);

    procedure callme(a: t_byte_array; h, l : natural) is
    begin
        report "Called with t_byte_array(" & to_string(a'high) & ":" & to_string(a'low)  & ")";
        assert a'high = h;
        assert a'low = l;
    end;

    function gen_data(n: integer) return t_byte_array is
        variable v_result : t_byte_array(0 to n-1);
    begin
        for i in 0 to n-1 loop
            v_result(i) := std_ulogic_vector(to_unsigned(i, 8));
        end loop;
        return v_result;
    end function;

begin


    test_proc: process
        variable v_src_bytes : t_byte_array(0 to 0);
        variable v_res_bytes : t_byte_array(0 to 1);
    begin

        -- Crash
        callme(gen_data(1) & x"--", 1, 0);

        -- Crash
        v_res_bytes := gen_data(1) & x"--";
        callme(v_res_bytes, 1, 0);

        -- Crash
        v_src_bytes(0) := x"--";
        v_res_bytes := gen_data(1) & v_src_bytes;
        callme(v_res_bytes, 1, 0);

        -- Works
        v_src_bytes(0) := x"--";
        v_res_bytes := v_src_bytes & gen_data(1);
        callme(v_res_bytes, 1, 0);

        -- Works
        v_src_bytes := gen_data(1);
        callme(v_src_bytes & x"--", 1, 0);

        -- Works
        v_src_bytes := gen_data(1);
        v_res_bytes := v_src_bytes & x"--";
        callme(v_res_bytes, 1, 0);

        wait;
    end process test_proc;


end architecture rtl;
