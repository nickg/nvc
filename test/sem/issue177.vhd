entity ent is
end entity;

architecture a of ent is
begin
  main : process
    function fun return integer is
    begin
      wait for 1 ns;
      return 42;
    end function;
  begin
  end process;
end architecture;
