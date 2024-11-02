package foo_pkg is
  signal sig: integer;
end foo_pkg;

package body foo_pkg is
  package bar_pkg is
    signal sig: integer;
  end bar_pkg;
  package body bar_pkg is
  end package body bar_pkg;
end package body foo_pkg;

package foo is
  alias TO_OCTAL_STRING is ns;
end package foo;
