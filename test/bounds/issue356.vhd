entity nvc_bug is
end nvc_bug;

architecture behav of nvc_bug is
    type std_logic_vector is array (integer range <>) of integer;

    function to_bitvector(x : std_logic_vector) return bit_vector is
    begin
        return "";
    end function;

    signal mode : std_logic_vector(1 downto 0);

begin

    process
        subtype modetype is bit_vector(mode'range);
    begin
        case modetype'(to_bitvector(mode)) is
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
