library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue820 is
generic (
  G_NUM_ARR                     : integer := 4;
  G_WIDTH                      : integer := 4*8
);
port (
  clk                          : in std_logic;
  o_dout                       : out std_logic_vector(G_WIDTH-1 downto 0) := (others => '0')
);
end issue820;

architecture rtl of issue820 is
  type t_arr_property is array (natural range<>) of natural;

  function num_arr (g_num_arr : natural) return t_arr_property is
    variable width_arr : t_arr_property(0 to g_num_arr-1) := (others => 8);
  begin
    return(width_arr);
  end function;

  function f_bit_low (width_arr : t_arr_property) return t_arr_property is
    variable arr_bit_low : t_arr_property(width_arr'range) := (others => 0);
  begin
    for i in width_arr'range loop
      if(i=0) then
        arr_bit_low(i) := 0;
      else
        arr_bit_low(i) := arr_bit_low(i-1) + width_arr(i-1);
      end if;
    end loop;
    return(arr_bit_low);
  end function;

  function f_bit_high (width_arr : t_arr_property) return t_arr_property is
    variable arr_bit_high : t_arr_property(width_arr'range) := (others => 0);
  begin
    for i in width_arr'range loop
      if(i=0) then
        arr_bit_high(i) := -1 + width_arr(i);
      else
        arr_bit_high(i) := arr_bit_high(i-1) + width_arr(i);
      end if;
    end loop;
    --
    return(arr_bit_high);
  end function;

  constant C_WIDTH_ARR     : t_arr_property := num_arr(G_NUM_ARR);
  constant C_BIT_LOW       : t_arr_property := f_bit_low(C_WIDTH_ARR);
  constant C_BIT_HIGH      : t_arr_property := f_bit_high(C_WIDTH_ARR);

  signal s_clk : std_logic := '0';
  signal s_dout, s_dout2 : std_logic_vector(G_WIDTH-1 downto 0);

begin

  p_clk: process
  begin
    wait for 100 ns;
    s_clk <= '1';
    wait for 100 ns;
    assert s_dout = X"01010101";
    assert s_dout2 = X"01010101";
    s_clk <= '0';
    wait for 100 ns;
    s_clk <= '1';
    wait for 100 ns;
    assert s_dout = X"02020202";
    assert s_dout2 = X"02020202";
    s_clk <= '0';
    wait;
  end process;

  gen_arr: for i in 0 to G_NUM_ARR-1 generate
    signal  data   : unsigned(C_WIDTH_ARR(i)-1 downto 0) := (others => '0');
  begin

    p_dummy: process(s_clk)
      begin
       if (rising_edge(s_clk)) then
        data <= data + 1;
      end if;
    end process;
    --
    gen_reg: if(True) generate
      s_dout(C_BIT_HIGH(i)  downto C_BIT_LOW(i)) <= std_logic_vector(data);
      s_dout2(C_BIT_HIGH(i to i)(i)  downto C_BIT_LOW(i to i)(i)) <= std_logic_vector(data);
    end generate;
  end generate gen_arr;

  o_dout <= s_dout;

end architecture rtl;
