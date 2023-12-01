package test_pkg is

    type vector_of_vectors is array(natural range <>) of bit_vector ;

end package ;

use work.test_pkg.vector_of_vectors ;

entity test is
  port (
    in_a    :   in  vector_of_vectors ;
    in_b    :   in  vector_of_vectors ;
    outs    :   out vector_of_vectors
  ) ;
end entity ;

architecture arch of test is

    -- Comment these next lines to cause a segfault
    constant A : natural := in_a'length ;
    constant B : natural := in_a'element'length ;

begin

    process
    begin
        report "A: " & integer'image(A) ;
        report "B: " & integer'image(B) ;
        assert A = 2 ;
        assert B = 21 ;
        wait ;
    end process ;

    process(all)
    begin
        for idx in in_a'range loop
            for x in in_a(idx)'range loop
                outs(idx)(x) <= in_a(idx)(x) xor in_b(idx)(x) ;
            end loop ;
        end loop ;
    end process ;

end architecture ;

use work.test_pkg.vector_of_vectors ;

entity issue809 is
end entity ;

architecture arch of issue809 is

    signal a : vector_of_vectors(1 downto 0)(20 downto 0) ;
    signal b : vector_of_vectors(1 downto 0)(20 downto 0) ;
    signal c : vector_of_vectors(1 downto 0)(20 downto 0) ;

begin

    U_test_a : entity work.test
      port map (
        in_a    =>  a,
        in_b    =>  b,
        outs    =>  c
      ) ;

    tb : process
    begin
        a <= (others => (others => '0'));
        b <= (others => (others => '1'));
        wait for 100 ns ;
        assert c = (1 downto 0 => (20 downto 0 => '1'));
        report "Done" ;
        std.env.stop ;
    end process ;

end architecture ;
