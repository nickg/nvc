entity issue1301 is begin
end entity issue1301;

architecture arch of issue1301 is
    procedure proc (
        variable b : out bit_vector;
        variable c : out bit
    ) is begin
        assert false severity failure;
    end procedure;
    attribute foreign of proc : procedure is "GHDL ./t.so issue1301_proc_c";
begin
    process is
        variable b : bit_vector(4 downto 0);
        variable c : bit;
    begin
        proc(b, c);
        assert c = '0';
        b(2) := '1';
        proc(b, c);
        assert c = '1';
        wait;
    end process;
end architecture arch;
