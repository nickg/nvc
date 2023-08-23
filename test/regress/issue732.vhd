package test_pkg is

    type test_t is record
        data : bit_vector ;
    end record ;

    type config_t is record
        width : positive ;
    end record ;

    constant DEFAULT_CONFIG : config_t := config_t'(width => 10) ;

    package make is
      generic (
        CONFIG : config_t := DEFAULT_CONFIG
      ) ;

        subtype sub_test_t is test_t(data(CONFIG.WIDTH-1 downto 0)) ;

    end package ;

end package ;

package newtest is new work.test_pkg.make ;

entity issue732 is
end entity ;

architecture arch of issue732 is

    signal x : work.newtest.sub_test_t ;

begin

    assert x.data'length = 10;

end architecture ;
