entity signal8 is
end entity;

architecture test of signal8 is
    type int_array is array (integer range <>) of integer;
    type int_array_Nx4 is array (integer range <>) of int_array(1 to 4);
    
    signal a : int_array_Nx4(1 to 4);
    signal b : int_array(1 to 4);
begin

    process is
    begin
  --      a(1)(2) <= 2;
        --assert a(1)(2) = 2;
--        a := ( others => ( 1, 2, 3, 4 ) );
--        b := a(1);
--        assert b = ( 1, 2, 3, 4);
        wait;
    end process;

end architecture;
