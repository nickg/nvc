package test_pkg is
	type array_of_vectors is array(integer range 0 to 31) of bit_vector(7 downto 0);
end package;

use work.test_pkg.array_of_vectors ;

entity test is
  port (
    in_a    :   in  array_of_vectors ;
    outs    :   out array_of_vectors
  ) ;
end entity ;

architecture arch of test is
begin
    process(all)
    begin
        outs(0) <= in_a(0);
    end process ;
end architecture ;

use work.test_pkg.array_of_vectors ;

entity issue817 is
end entity ;

architecture arch of issue817 is

    signal a : bit;
    signal b : bit_vector(7 downto 0);

begin

    U_test_a : entity work.test
      port map (
        in_a(0)    =>  "1111111" & a,  	-- OK
        in_a(1)    =>  "111111" & a,  	-- Error
        outs(0)    => b
      ) ;

    tb : process
    begin
        wait for 100 ns ;
        report "Done" ;
        std.env.stop ;
    end process ;

end architecture ;
