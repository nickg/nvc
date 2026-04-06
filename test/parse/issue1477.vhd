package A_pkg is

  generic (type C_type);

  type A is protected
  end protected;

end package;

package body A_pkg is

  type A is protected body

    type C_ptr is access C_type;
    type D;
    type D is record
      C : C_ptr;
    end record;

  end protected body;

end package body;

package B is new work.A_pkg
  generic map(bit_vector(7 downto 0));  -- OK
