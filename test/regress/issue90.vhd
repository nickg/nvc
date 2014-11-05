entity issue90 is
end entity;

architecture test of issue90 is
  procedure proc(x : inout integer) is
    procedure nested_p1(x : inout integer) is
    begin
        x := x + 1;
    end;

    procedure nested_p2(x : inout integer) is
    begin
      nested_p1(x);
      x := x + 1;
    end;
  begin
      nested_p2(x);
      x := x + 1;
  end procedure;
begin

    process is
        variable v : integer := 0;
    begin
        proc(v);
        assert v = 3;
        wait;
    end process;

end architecture;
