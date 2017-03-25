-- test_ng.vhd
entity  issue309 is
end     issue309;
architecture MODEL of issue309 is

    function check return boolean is
        constant org_data  : bit_vector(31 downto 0) := "01110110010101000011001000010000";
        variable val_data  : bit_vector(31 downto 0);
    begin
        val_data := (others => '0');
        for i in 7 downto 0 loop
            val_data(31 downto 4) := val_data(27 downto 0);
            val_data( 3 downto 0) := org_data(i*4+3 downto i*4);
        end loop;
        return (val_data = org_data);
    end function;

    function check2 return boolean is
        constant org_data  : bit_vector(0 to 31) := "01110110010101000011001000010000";
        variable val_data  : bit_vector(0 to 31);
    begin
        val_data := (others => '0');
        for i in 7 downto 0 loop
            val_data(4 to 31) := val_data(0 to 27);
            val_data(0 to 3) := org_data(i*4 to i*4+3);
        end loop;
        return (val_data = org_data);
    end function;

    constant c : boolean := check;
    constant d : boolean := check2;

begin

    g1: if c generate
        assert true report "c";
    end generate;

    g2: if d generate
        assert true report "d";
    end generate;

end MODEL;
