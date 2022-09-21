entity test is
end entity test;
architecture beh of test is
  type t_vvc_config is
  record
    clock_name : string(1 to 30);
  end record;
  signal vvc_config : t_vvc_config;
  alias clock_name      : string is vvc_config.clock_name;
begin

  process
  begin
    vvc_config.clock_name <= (others => NUL);
    clock_name <= (others => NUL);
    wait;
  end process;

end architecture beh;
