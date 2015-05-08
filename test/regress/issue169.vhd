entity issue169 is
end entity;

architecture a of issue169 is
begin
  main : process
    procedure proc(x : natural) is
    begin
        if x > 0 then
            wait for 1 ns;
            proc(x - 1);
        end if;
    end procedure;
  begin
      proc(5);
      assert now = 5 ns;
      wait;
  end process;
end architecture;
