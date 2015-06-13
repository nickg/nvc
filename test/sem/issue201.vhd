package pkg is
  type prot_t is protected
    procedure proc;
  end protected;
end package;

package body pkg is
  type prot_t is protected body

    procedure proc is
      procedure nested_proc is
      begin
      end procedure;

      function nested_fun return integer is
      begin
        return 0;
      end function;

      function nested_ifun return integer is
      begin
        return 0;
      end function;
    begin
    end procedure;
  end protected body;
end package body;
