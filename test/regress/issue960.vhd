entity issue960 is
end entity;

architecture arch of issue960 is
    type T_RECORD is record
        EL : bit;
    end record T_RECORD;

    type T_ARRAY is array (1 downto 0) of T_RECORD;

    signal array_sig : T_ARRAY;

    signal stable_sig : boolean;
begin
    stable_sig <= array_sig'stable;

    check: process is
    begin
        assert not stable_sig;
        wait for 0 ns;
        assert stable_sig;
        array_sig(1).el <= '1' after 1 ns;
        wait for 1 ns;
        assert stable_sig;
        wait for 0 ns;
        assert not stable_sig;
        wait for 0 ns;
        assert stable_sig;
        wait;
    end process;

end architecture arch;
