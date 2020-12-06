package pack is
    procedure proc;
end package;

package body pack is

    procedure proc is
    begin
        wait for 1 ns;
    end procedure;

end package body;

-------------------------------------------------------------------------------

entity ent is
end entity;

use work.pack.all;

architecture a of ent is
begin
  main : process
    procedure proc_wait is
    begin
      wait for 1 ns;
    end procedure;

    procedure proc_wait_indirect is
    begin
      proc_wait;
    end procedure;

    impure function fun return integer is
    begin
      proc_wait_indirect;               -- Error
      proc_wait;                        -- Error
      return 42;
    end function;
    variable var : integer;
  begin
    var := fun;
    assert var = 42;
    report integer'image(var);
    wait until false;
  end process;

  process is
      function fun2 return boolean is
      begin
          proc;                         -- Error (not at sem)
          return true;
      end function;
  begin
      assert fun2;
      wait;
  end process;
end architecture;
