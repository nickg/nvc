entity test is
end entity test;
architecture beh of test is
    signal sig : bit_vector(-1 downto 0);
    signal sig2 : bit_vector(0 to -1);
begin
    process(sig,sig2)
    begin
        sig <= (sig'range => '0');            -- OK
        sig2 <= (sig2'range => '0');          -- OK
    end process;

    process is
        type t_null_int is range 0 to -1;
        variable x : t_null_int := 0;         -- Error
    begin
    end process;
end architecture beh;
