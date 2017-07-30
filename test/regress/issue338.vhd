entity issue338 is
end entity;

architecture a of issue338 is
  signal pos : natural := 4;
  signal divisor : natural := 0;
begin
  pure_function : process
    function fail return boolean is
    begin
      assert false report "Should never be reached" severity failure;
      return true;
    end;

    procedure proc(value : boolean; expected : boolean) is
    begin
      assert value = expected severity failure;
    end;


    variable count : natural := 0;
    variable value : boolean;
  begin

    while false and fail loop
      assert false report "Should never happen" severity error;
      count := count + 1;
    end loop;

    while true or fail loop
      count := count + 2;
      exit;
    end loop;

    if false and fail then
      assert false report "Should never happen" severity error;
      count := count + 4;
    end if;

    if true or fail then
      count := count + 8;
    end if;

    loop
      exit when true or fail;
      count := count + 16;
    end loop;

    loop
      exit when false and fail;
      count := count + 32;
      exit;
    end loop;

    proc(false and fail, false);
    proc(true or fail, true);

    value := true or fail;
    assert value;
    value := false and fail;
    assert not value;

    assert true or fail;
    assert not (false and fail);

    report integer'image(count);
    assert count = 2+8+32 severity failure;
    report "Finished";
    wait;
  end process;

  impure_function : process
    variable retval : boolean := false;

    impure function fail return boolean is
    begin
      assert false report "Should never be reached" severity failure;
      retval := not retval;
      return retval;
    end;

    procedure proc(value : boolean; expected : boolean) is
    begin
      assert value = expected severity failure;
    end;


    variable count : natural := 0;
    variable value : boolean;
  begin

    while false and fail loop
      assert false report "Should never happen" severity error;
      count := count + 1;
    end loop;

    while true or fail loop
      count := count + 2;
      exit;
    end loop;

    if false and fail then
      assert false report "Should never happen" severity error;
      count := count + 4;
    end if;

    if true or fail then
      count := count + 8;
    end if;

    loop
      exit when true or fail;
      count := count + 16;
    end loop;

    loop
      exit when false and fail;
      count := count + 32;
      exit;
    end loop;

    proc(false and fail, false);
    proc(true or fail, true);

    value := true or fail;
    assert value;
    value := false and fail;
    assert not value;

    assert true or fail;
    assert not (false and fail);

    report integer'image(count);
    assert count = 2+8+32 severity failure;
    report "Finished";
    wait;
  end process;

  access_out_of_range : process
    constant s : string := "123";

    procedure proc(value : boolean; expected : boolean) is
    begin
      assert value = expected severity failure;
    end;

    variable count : natural := 0;
    variable value : boolean;
  begin

    while false and s(pos) = '1' loop
      assert false report "Should never happen" severity error;
      count := count + 1;
    end loop;

    while true or s(pos) = '1' loop
      count := count + 2;
      exit;
    end loop;

    if false and s(pos) = '1' then
      assert false report "Should never happen" severity error;
      count := count + 4;
    end if;

    if true or s(pos) = '1' then
      count := count + 8;
    end if;

    loop
      exit when true or s(pos) = '1';
      count := count + 16;
    end loop;

    loop
      exit when false and s(pos) = '1';
      count := count + 32;
      exit;
    end loop;

    proc(false and s(pos) = '1', false);
    proc(true or s(pos) = '1', true);

    value := true or s(pos) = '1';
    assert value;
    value := false and s(pos) = '1';
    assert not value;

    assert true or s(pos) = '1';
    assert not (false and s(pos) = '1');

    report integer'image(count);
    assert count = 2+8+32 severity failure;
    report "Finished";
    wait;
  end process;

  non_static_access_out_of_range : process
    constant s : string := "123";

    procedure proc(value : boolean; expected : boolean) is
    begin
      assert value = expected severity failure;
    end;

    variable count : natural := 0;
    variable value : boolean;
    variable pos : natural;
  begin
    pos := 4;

    while pos < 4 and s(pos) = '1' loop
      assert false report "Should never happen" severity error;
      count := count + 1;
    end loop;

    while pos = 4 or s(pos) = '1' loop
      count := count + 2;
      exit;
    end loop;

    if pos < 4 and s(pos) = '1' then
      assert false report "Should never happen" severity error;
      count := count + 4;
    end if;

    if pos = 4 or s(pos) = '1' then
      count := count + 8;
    end if;

    loop
      exit when pos = 4 or s(pos) = '1';
      count := count + 16;
    end loop;

    loop
      exit when pos < 4 and s(pos) = '1';
      count := count + 32;
      exit;
    end loop;

    proc(pos < 4 and s(pos) = '1', false);
    proc(pos = 4 or s(pos) = '1', true);

    value := pos = 4 or s(pos) = '1';
    assert value;
    value := pos < 4 and s(pos) = '1';
    assert not value;

    assert pos = 4 or s(pos) = '1';
    assert not (pos < 4 and s(pos) = '1');

    report integer'image(count);
    assert count = 2+8+32 severity failure;
    report "Finished";
    wait;
  end process;

  divide_by_zero : process
  begin
    assert true or 7/divisor = 1;
    assert not (false and 7/divisor = 1);
    wait;
  end process;

  pure_but_expensive : process
    function expensive return boolean is
      variable result : integer;
    begin
      for i in 0 to 2**29 loop
        for j in 0 to 2**29 loop
          for k in 0 to 2*29 loop
            result := ((result+k-i+j) mod 2**19) * (k mod 2**10);
          end loop;
        end loop;
      end loop;
      return (result mod 16) = 0;
    end;
  begin
    assert true or expensive;
    assert not (false and expensive);
    wait;
  end process;

end;
