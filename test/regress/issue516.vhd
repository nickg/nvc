library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue516 is
end entity;
architecture beh of issue516 is
  type t_event_ctrl_unresolved is record
    val : std_logic;
    ack : std_logic;
  end record;
  type t_event_ctrl_drivers is array (natural range <> ) of t_event_ctrl_unresolved;
  function resolved_event_ctrl(input_vector : t_event_ctrl_drivers) return t_event_ctrl_unresolved;
  subtype t_event_ctrl is resolved_event_ctrl t_event_ctrl_unresolved;

  type t_events_unresolved is record
    flag1   : t_event_ctrl;
    flag2   : t_event_ctrl;
  end record;
  type t_events_drivers is array (natural range <> ) of t_events_unresolved;
  function resolved_events(input_vector : t_events_drivers) return t_events_unresolved;
  subtype t_events is resolved_events t_events_unresolved;
  function resolved_event_ctrl(
    input_vector : t_event_ctrl_drivers
  ) return t_event_ctrl_unresolved
  is
    variable ret : t_event_ctrl_unresolved := (others => '0');
  begin
    if input_vector'length = 0 THEN
      return ret;
    else
      for i in input_vector'range loop
        if input_vector(i).val = '1' then
          ret.val := '1';
        end if;
        if input_vector(i).ack = '1' then
          ret.ack := '1';
        end if;
      end loop;
      return ret;
    end if;
  end function resolved_event_ctrl;

  function resolved_events(
    input_vector : t_events_drivers
  ) return t_events_unresolved
  is
    variable ret : t_events_unresolved := (others => (others => '0'));
  begin
    if input_vector'length = 0 THEN
      return ret;
    else
      for i in input_vector'range loop
        ret.flag1  := resolved_event_ctrl(t_event_ctrl_drivers'(ret.flag1, input_vector(i).flag2));
        ret.flag2   := resolved_event_ctrl(t_event_ctrl_drivers'(ret.flag2, input_vector(i).flag2));
      end loop;
      return ret;
    end if;
  end function resolved_events;
  signal events : t_events;
  signal passed : boolean;
begin
  p_events : process
  begin
    events <= (others => (others => '0'));
    loop
      wait on events.flag2.val;
      report "Got val" severity note;
      if rising_edge(events.flag2.val) then
        report "Sending ack" severity note;
        events.flag2.ack           <= '1' after 1 us, '0' after 1.1 us;
      end if;
      if falling_edge(events.flag2.val) then
        report "Resetting ack" severity note;
        events.flag2.ack           <= '1', '0' after 1 ns;
      end if;
    end loop;
  end process p_events;

  process
    procedure com2(signal flag : inout t_event_ctrl) is
    begin
      events.flag2.val <= '1';
      report "Send val 1" severity note;
      wait until events.flag2.ack = '1';
      report "Got ack" severity note;
      report "Send val 0" severity note;
      events.flag2.val <= '1';
      wait until events.flag2.ack = '0';
      report "Got ack 0" severity note;
    end procedure;
    procedure com(signal flag_event : inout t_events) is
    begin
      com2(flag_event.flag2);
    end procedure;
  begin
    wait for 1 us;
    report "Running" severity note;
    com(events);
    passed <= true;
    wait;
  end process;

  check: process is
  begin
      wait for 5 us;
      assert passed;
      wait;
  end process;

end architecture beh;
