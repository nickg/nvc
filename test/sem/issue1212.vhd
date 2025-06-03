package tb_utils is
end package tb_utils;

package body tb_utils is
  procedure f_gen_clk(signal   clk  : out bit;
                      variable clk2 : out bit;
                      constant freq : in  natural) is
  begin
    loop
      wait for (0.5 / real(freq)) * 1 sec;
      clk <= not clk;                   -- Error (even in 2008)
      clk2 := not clk2;                 -- OK (in 2008)
    end loop;
  end procedure f_gen_clk;
end package body;
