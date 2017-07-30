entity issue338b is
end entity;

architecture a of issue338b is
begin
  main : process
    procedure proc(s : string; variable value : out integer) is
    begin
      if s = "" then
        value := 0;
      else
        value := 1;
      end if;
    end;

    function func(s : string) return integer is
    begin
      if s = "" then
        return 0;
      end if;

      return 1;
    end;

    constant s1 : string := "foobar";
    constant s0 : string := "";

    variable value : integer;
  begin
    assert func(s1) = 1;
    assert func(s0) = 0;

    proc(s1, value);
    assert value = 1;

    proc(s0, value);
    assert value = 0;
    wait;
  end process;
end;
