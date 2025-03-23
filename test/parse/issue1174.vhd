package Test_P is
    type Test_R_t is record
        s_1 : bit_vector;
        s_2 : bit_vector;
    end record Test_R_t;

    type Test_A_t is array(natural range <>) of Test_R_t;

    function clog2 (val: integer) return integer;
end package;

-------------------------------------------------------------------------------

use work.Test_p.all;

entity Test is
  generic(
    g_Test_1 : integer;
    g_Test_2 : integer
  );
  port(
    clk : in bit;
    test : out Test_A_t(0 to g_Test_1)(
      s_1(clog2(g_Test_2)-1 downto 0),
      s_2(clog2(g_Test_2)-1 downto 0)
    )
  );
end entity Test;

architecture Behavioral of Test is
begin

  process(all) is
    variable v_test : test'subtype;     -- OK (should not call clog2)
  begin
  end process;

end architecture;
