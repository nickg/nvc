entity ent is
end entity;

architecture a of ent is
  type protected_t is protected
    procedure proc;
  end protected;

  type protected_t is protected body
    procedure proc is
    begin
      report "tick";
      wait for 1 ns;
      report "tock";
    end procedure;
  end protected body;

begin
  main : process
    variable prot : protected_t;
  begin
    prot.proc;
    wait;
  end process;
end architecture;
