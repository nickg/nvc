-- test_ng.vhd
use     std.textio.all;
entity  issue309 is
end     issue309;
architecture MODEL of issue309 is
begin
    process
        constant org_data  : bit_vector(31 downto 0) := "01110110010101000011001000010000";
        variable val_data  : bit_vector(31 downto 0);
        variable text_line : LINE;
    begin
        val_data := (others => '0');
        for i in 7 downto 0 loop
            write(text_line, string'("val_data="));
            write(text_line, val_data);
            val_data(31 downto 4) := val_data(27 downto 0);
            val_data( 3 downto 0) := org_data(i*4+3 downto i*4);
            write(text_line, string'("=>"));
            write(text_line, val_data);
            writeline(OUTPUT, text_line);
        end loop;
        if (val_data /= org_data) then
            write(text_line, string'("NG:org_data="));
            write(text_line, org_data);
            writeline(OUTPUT, text_line);
            write(text_line, string'("   val_data="));
            write(text_line, val_data);
            writeline(OUTPUT, text_line);
            assert false;
        end if;
        wait;
    end process;
end MODEL;
