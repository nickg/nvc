entity issue348 is
end entity;

architecture a of issue348 is
begin
  main : process
  begin
    std.env.stop(0);
    assert False severity failure;
  end process;
end;
