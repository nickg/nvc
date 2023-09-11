entity dummy_unit is
  port(
      -- coverage off
      clk            : in  bit; -- slow clk
      o_port         : out bit_vector(1 downto 0)
  );
end entity dummy_unit;


architecture rtl of dummy_unit is

begin

    -- pragma translate_off
    o_port(0) <= clk;

 -- synthesis translate_off
    o_port(1) <= clk;

end architecture rtl;
