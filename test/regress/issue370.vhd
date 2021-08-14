-- -*- vhdl-basic-offset: 2 -*-
package run_types_pkg is
  type runner_phase_unresolved_t is (test_runner_entry, test_runner_setup, test_suite_setup, test_case_setup, test_case, test_case_cleanup, test_suite_cleanup, test_runner_cleanup, test_runner_exit, multiple_drivers);
  type runner_phase_unresolved_array_t is array (integer range <>) of runner_phase_unresolved_t;
  function resolve_runner_phase (
    constant values : runner_phase_unresolved_array_t)
    return runner_phase_unresolved_t;
  subtype runner_phase_t is resolve_runner_phase runner_phase_unresolved_t;

  type phase_locks_unresolved_t is record
    entry_is_locked : boolean;
    exit_is_locked : boolean;
  end record phase_locks_unresolved_t;
  type phase_locks_unresolved_array_t is array (integer range <>) of phase_locks_unresolved_t;
  function resolve_phase_locks (
    constant values : phase_locks_unresolved_array_t)
    return phase_locks_unresolved_t;
  subtype phase_locks_t is resolve_phase_locks phase_locks_unresolved_t;
  type phase_locks_array_t is array (runner_phase_t range <>) of phase_locks_t;

  type boolean_array_t is array (integer range <>) of boolean;
  function resolve_runner_flag (
    constant values : boolean_array_t)
    return boolean;
  subtype runner_flag_t is resolve_runner_flag boolean;

  type runner_sync_t is record
    phase : runner_phase_t;
    locks : phase_locks_array_t(test_runner_setup to test_runner_cleanup);
    exit_without_errors : runner_flag_t;
  end record runner_sync_t;
end package;

package body run_types_pkg is
  function resolve_runner_phase (
    constant values : runner_phase_unresolved_array_t)
    return runner_phase_unresolved_t is
    variable n_set_values : natural := 0;
    variable result : runner_phase_unresolved_t := test_runner_entry;
  begin
    report "resolve_runner_phase called";
    for i in values'range loop
      if values(i) = test_runner_exit then
        return test_runner_exit;
      elsif values(i) /= test_runner_entry then
        result := values(i);
        n_set_values := n_set_values + 1;
      end if;
    end loop;

    if n_set_values > 1 then
      result := multiple_drivers;
    end if;

    return result;
  end;

  function resolve_phase_locks (
    constant values : phase_locks_unresolved_array_t)
    return phase_locks_unresolved_t is
    variable result : phase_locks_t;
  begin
    report "resolve_phase_locks called";
    result.entry_is_locked := false;
    result.exit_is_locked := false;
    for i in values'range loop
      if values(i).entry_is_locked then
        result.entry_is_locked := true;
      end if;
      if values(i).exit_is_locked then
        result.exit_is_locked := true;
      end if;
    end loop;

    return result;
  end;

  function resolve_runner_flag (
    constant values : boolean_array_t)
    return boolean is
  begin
    for i in values'range loop
      if values(i) = true then
        return true;
      end if;
    end loop;

    return false;
  end;

end package body run_types_pkg;


use work.run_types_pkg.all;

entity issue370 is
end entity;

architecture a of issue370 is
  signal runner : runner_sync_t;

  procedure drive(signal runner: inout runner_sync_t; value : boolean) is
  begin
    runner.exit_without_errors <= value;
  end;
begin

  p0 : process
  begin
    drive(runner, false);
    wait;
  end process;

  p1 : process
  begin
    drive(runner, true);
    wait;
  end process;

  p2 : process
  begin
    drive(runner, false);
    wait;
  end process;

  check_p: process is
  begin
      wait for 1 ns;
      assert runner.exit_without_errors;
      wait;
  end process;
end;
