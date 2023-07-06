entity issue734 is
end entity ;

architecture arch of issue734 is
    signal val : bit_vector(0 downto 1) ;
begin

    tb : process
        variable n : integer := 0;
    begin
        wait for 1 ns;
        val(n) <= '0' ;                 -- Error
        wait ;
    end process ;

end architecture ;
