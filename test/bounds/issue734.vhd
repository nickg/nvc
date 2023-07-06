entity issue734 is
end entity ;

architecture arch of issue734 is
    signal val : bit_vector(0 downto 1) ;
begin

    tb : process
    begin
        val(0) <= '0' ;                 -- Error
        val(1 downto 0) <= "00";        -- Error
        wait ;
    end process ;

end architecture ;
