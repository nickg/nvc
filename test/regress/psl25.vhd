library ieee;
use ieee.std_logic_1164.all;

entity psl25 is
end entity;

architecture tb of psl25 is

    signal clk : natural;

    signal a : bit := '1';
    signal b : bit := '1';
    signal c : bit := '1';

    signal s_bit        : bit := '1';
    signal s_num        : integer := 42;
    signal s_vect       : bit_vector(3 downto 0) := "1010";
    signal s_string     : string(1 to 5) := "HELLO";

    type t_enum is (
        ENUMERATOR_1,
        ENUMERATOR_2,
        ENUMERATOR_3,
        ENUMERATOR_4
    );
    signal s_enum       : t_enum := ENUMERATOR_3;

    constant fake_str   : string(1 to 3) := "ABC";
begin

    process
    begin
        wait for 500 ps;
    	for i in 0 to 3 loop
        	clk <= clk + 1;
        	wait for 1 ns;
        end loop;
        wait;
    end process;

    -- psl default clock is clk'event;

    ---------------------------------------------------------------------------
    -- Sequence declarations
    ---------------------------------------------------------------------------
    -- psl sequence seq_with_bit(bit b) is {b = '1';b};
    -- psl sequence seq_with_numeric(numeric n) is {n = 42;b};
    -- psl sequence seq_with_vector(bitvector bv) is {bv = "1010";b};
    -- psl sequence seq_with_string(string s) is {s = "HELLO";b};
    -- psl sequence seq_with_hdltype(hdltype t_enum hdl) is {hdl = ENUMERATOR_3;b};

    ---------------------------------------------------------------------------
    -- Assertions - All should fail
    ---------------------------------------------------------------------------

    -- Should fail at 3500 ps
    -- psl assert never {a;seq_with_bit(s_bit);c} report "seq_with_bit(s_bit) failed";
    -- TODO: Passing '1' directly and std_logic fails due to
    --       "bit" vs "std_logic" resolution

    -- Should fail at 3500 ps
    -- psl assert never {a;seq_with_numeric(42);c} report "seq_with_numeric(42) failed";
    -- psl assert never {a;seq_with_numeric(s_num);c} report "seq_with_numeric(s_num) failed";

    -- Should fail at 3500 ps
    -- psl assert never {a;seq_with_vector(s_vect);c} report "seq_with_vector(s_vect) failed";
    -- TODO: Passing "1010" or std_logic_vecto fails due to
    --       "bit_vector" vs "std_logic_vector" resolution

    -- Should fail at 3500 ps
    -- psl assert never {a;seq_with_hdltype(s_enum);c} report "seq_with_hdltype(s_enum) failed";

    ---------------------------------------------------------------------------
    -- Assertions - None should fail
    ---------------------------------------------------------------------------
    -- psl assert never {seq_with_bit(to_bit('0'));c} report "seq_with_bit(to_bit('0') failed";
    -- psl assert never {seq_with_numeric(58);c} report "seq_with_numeric(58) failed";
    -- psl assert never {seq_with_vector(to_bit_vector("1011"));c} report "seq_with_vector(to_bit_vector(1011)) failed";
    -- psl assert never {seq_with_string(fake_str);c} report "seq_with_string(fake_str) failed";
    -- psl assert never {seq_with_hdltype(ENUMERATOR_2);c} report "seq_with_hdltype(ENUMERATOR_2) failed";

end architecture;
