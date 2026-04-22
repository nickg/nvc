library ieee;
use ieee.std_logic_1164.all;

entity issue1512_inner is
  port (
    clk       : in std_logic;
    rst       : in std_logic;
    in_valid  : in std_logic;
    in_ready  : out std_logic;
    in_data   : in std_logic_vector(3 downto 0);
    out_valid : out std_logic;
    out_ready : in std_logic := '1';
    out_data  : out std_logic_vector(3 downto 0)
  );
end entity;

architecture rtl of issue1512_inner is
  type two_process_r is record
    data_main     : std_logic_vector(3 downto 0);
    data_main_vld : std_logic;
    data_shad     : std_logic_vector(3 downto 0);
    data_shad_vld : std_logic;
    in_ready_f    : std_logic;
  end record;

  signal r, r_next : two_process_r;
begin

  p_comb : process (all) is
    variable v : two_process_r;
    variable is_stuck_v : boolean;
  begin
    v := r;
    is_stuck_v := r.data_main_vld = '1'
                  and out_ready = '0'
                  and (in_valid = '1' or r.data_shad_vld = '1');

    if r.data_main_vld = '1' and out_ready = '1' then
      v.data_main_vld := r.data_shad_vld;
      v.data_main := r.data_shad;
      v.data_shad_vld := '0';
    end if;

    if r.in_ready_f = '1' and in_valid = '1' then
      if is_stuck_v then
        v.data_shad_vld := '1';
        v.data_shad := in_data;
      else
        v.data_main_vld := '1';
        v.data_main := in_data;
      end if;
    end if;

    if is_stuck_v then
      v.in_ready_f := '0';
    else
      v.in_ready_f := '1';
    end if;

    r_next <= v;
  end process;

  in_ready <= r.in_ready_f;
  out_valid <= r.data_main_vld;
  out_data <= r.data_main;

  p_seq : process (clk) is
  begin
    if rising_edge(clk) then
      r <= r_next;
      if rst = '1' then
        r.data_main_vld <= '0';
        r.data_shad_vld <= '0';
        r.in_ready_f <= '1';
      end if;
    end if;
  end process;

end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity issue1512_outer is
  port (
    clk       : in std_logic;
    rst       : in std_logic;
    in_valid  : in std_logic := '1';
    in_ready  : out std_logic;
    in_data   : in std_logic_vector(3 downto 0);
    out_valid : out std_logic;
    out_ready : in std_logic := '1';
    out_data  : out std_logic_vector(3 downto 0)
  );
end entity;

architecture rtl of issue1512_outer is
  type data_t is array (natural range <>) of std_logic_vector(3 downto 0);

  signal data_s  : data_t(0 to 9);
  signal valid_s : std_logic_vector(0 to 9);
  signal ready_s : std_logic_vector(0 to 9);
begin

  valid_s(0) <= in_valid;
  in_ready <= ready_s(0);
  data_s(0) <= in_data;

  g_stages : for i in 0 to 8 generate
    i_stg : entity work.issue1512_inner
      port map (
        clk => clk,
        rst => rst,
        in_valid => valid_s(i),
        in_ready => ready_s(i),
        in_data => data_s(i),
        out_valid => valid_s(i + 1),
        out_ready => ready_s(i + 1),
        out_data => data_s(i + 1)
      );
  end generate;

  out_valid <= valid_s(9);
  ready_s(9) <= out_ready;
  out_data <= data_s(9);

end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.env.all;

entity issue1512 is
end entity;

architecture tb of issue1512 is
  signal clk       : std_logic := '0';
  signal rst       : std_logic := '1';
  signal in_valid  : std_logic := '0';
  signal in_ready  : std_logic;
  signal out_valid : std_logic;
  signal out_ready : std_logic := '0';
  signal in_data   : std_logic_vector(3 downto 0) := (others => '0');
  signal out_data  : std_logic_vector(3 downto 0);
begin

  clk <= not clk after 5 ns;

  dut : entity work.issue1512_outer
    port map (
      clk => clk,
      rst => rst,
      in_valid => in_valid,
      in_ready => in_ready,
      in_data => in_data,
      out_valid => out_valid,
      out_ready => out_ready,
      out_data => out_data
    );

  stim : process is
  begin
    wait until rising_edge(clk);
    rst <= '0';
    wait until rising_edge(clk);

    in_valid <= '1';
    for i in 1 to 36 loop
      in_data <= std_logic_vector(to_unsigned(i mod 16, 4));
      if i = 24 then
        out_ready <= '1';
      end if;
      wait until rising_edge(clk);
    end loop;

    in_valid <= '0';
    wait for 20 ns;
    stop;
    wait;
  end process;

end architecture;
