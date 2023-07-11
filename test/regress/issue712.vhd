entity issue712 is
end;

architecture rtl of issue712 is

  signal a : character;
  signal b : character;
  signal c : character;
  signal k : string(1 to 5);
begin
  process
    variable  y : string(1 to 8);
  begin
    y := "abcdefgh";
    (1 => a, 2 => b, 3 => c, 4 to 8 => k) <= y;
    wait for 1 ns;
    report a & " " & b & " " & c &  " " & k;
    assert a = 'a';
    assert b = 'b';
    assert c = 'c';
    assert k = "defgh";
    wait;
  end process;

  process
    variable a : character;
    variable b : character;
    variable c : character;
    variable k : string(1 to 5);
    variable  y : string(1 to 8);
  begin
    y := "abcdefgh";
    (1 => a, 2 => b, 3 => c, 4 to 8 => k) := y;
    wait for 1 ns;
    assert a = 'a';
    assert b = 'b';
    assert c = 'c';
    assert k = "defgh";
    wait;
  end process;
end rtl;
