package p is
  type t_prot is protected
    procedure dummy;
  end protected;

  -- Instance declared BEFORE body
  shared variable obj : t_prot;

end package;

package body p is

  type t_prot is protected body

    procedure dummy is
    begin
      null;
    end procedure;

  end protected body;

end package body;
