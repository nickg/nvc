entity nvc_bug is
end nvc_bug;

architecture behav of nvc_bug is
    type std_logic_vector is array (integer range <>) of integer;

    function to_bitvector(x : std_logic_vector) return bit_vector;

    signal mode : std_logic_vector(1 downto 0);

begin

    process
    begin

        --nvc doesn't like the to_bitvector() below, fails in analysis.
        case to_bitvector(mode) is
            when "00" =>
            when "01" =>
            when "10" =>
            when "11" =>
            when others =>
        end case;

        assert false report "end of test" severity note;
        wait;
    end process;
end behav;
