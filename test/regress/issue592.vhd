-- -*- vhdl-basic-offset: 2 -*-
entity issue592 is
end entity issue592;
architecture beh of issue592 is
begin

  process
    variable v_len : natural := 31;
    variable zero : integer := 0;
    variable zero1 : natural := 0;
  begin
    wait for 1 ns;
    -- This does not overflow as it uses the universal_integer overload
    if 0 > 2 ** v_len - 1 then
      report "error" severity error;
    end if;
    if zero < -(2 ** (v_len - 1)) then  -- Uses the integer overload
      report "error" severity error;
    end if;
    zero := -(2 ** (v_len - 20));        -- Needs to truncate the result
    wait for 1 ns;
    assert zero = -2048;
    zero1 := -(2 ** (v_len - 20));        -- Error
    wait;
  end process;

end architecture beh;
