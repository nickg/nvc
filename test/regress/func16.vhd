entity func16 is
end entity func16;


architecture test of func16 is

    constant STATE_WORD_SIZE : integer := 8;

    type state_type is array (3 downto 0) of bit_vector(STATE_WORD_SIZE-1 downto 0);


    function TestFunction (StateInxD : state_type) return state_type is
        variable StateOutxD : state_type;
    begin
        StateOutxD := StateInxD;
        return StateOutxD;
    end function TestFunction;

begin

    process is
        variable r : state_type;
    begin
        r(0) := x"12";
        r(1) := x"34";
        r(2) := x"56";
        r(3) := x"78";

        r := TestFunction(r);

        assert r(0) = x"12" report "Vector mismatch!!";
        assert r(1) = x"34" report "Vector mismatch!!";
        assert r(2) = x"56" report "Vector mismatch!!";
        assert r(3) = x"78" report "Vector mismatch!!";
        wait;
    end process;

end architecture;
