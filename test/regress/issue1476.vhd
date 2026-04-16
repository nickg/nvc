LIBRARY ieee;
  USE ieee.std_logic_1164.all;

ENTITY test IS
  GENERIC (
    wmsb_depth : integer := 10
  );
  PORT (
    clk   : IN    std_logic;
    wrcnt : OUT   std_logic_vector(wmsb_depth DOWNTO 0)
  );
END ENTITY test;

ARCHITECTURE rtl OF test IS

  COMPONENT test_verilog IS
    GENERIC (
      wmsb_depth : integer
    );
    PORT (
      clk   : IN    std_logic;
      wrcnt : OUT   std_logic_vector(WMSB_DEPTH DOWNTO 0)
    );
  END COMPONENT test_verilog;

BEGIN

  inst_test : COMPONENT test_verilog
    GENERIC MAP (
      wmsb_depth => 10
    )
    PORT MAP (
      clk   => clk,
      wrcnt => wrcnt
    );

END ARCHITECTURE rtl;

-------------------------------------------------------------------------------

LIBRARY ieee;
  USE ieee.std_logic_1164.all;

ENTITY issue1476 IS
END ENTITY issue1476;

ARCHITECTURE rtl OF issue1476 IS

  CONSTANT wmsb_depth : integer := 10;

  SIGNAL clk   : std_logic := '0';
  SIGNAL wrcnt : std_logic_vector(wmsb_depth DOWNTO 0);

BEGIN

  clk <= NOT clk AFTER 10 ns when now < 100 ns;

  inst_test : ENTITY work.test
    GENERIC MAP (
      wmsb_depth => wmsb_depth
    )
    PORT MAP (
      clk   => clk,
      wrcnt => wrcnt
    );

END ARCHITECTURE rtl;
