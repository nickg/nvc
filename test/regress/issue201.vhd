package pkg is
  type prot_t is protected
    procedure proc(x : out integer);
  end protected;
end package;

package body pkg is
  type prot_t is protected body

    procedure proc(x : out integer) is
      function nested_fun return integer is
      begin
        return 5;
      end function;

      procedure nested_proc is
      begin
          x := nested_fun;
      end procedure;

      function nested_ifun return integer is
      begin
        return 0;
      end function;
    begin
        nested_proc;
    end procedure;
  end protected body;
end package body;

-------------------------------------------------------------------------------

entity issue201 is
end entity;

use work.pkg.all;

architecture test of issue201 is
    shared variable p : prot_t;
begin

    process is
        variable x : integer;
    begin
        p.proc(x);
        assert x = 5;
        wait;
    end process;

end architecture;
