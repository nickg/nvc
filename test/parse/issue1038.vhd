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

ENTITY psl_func_in_primary IS
END psl_func_in_primary;

ARCHITECTURE arch OF psl_func_in_primary IS
  TYPE integer_array IS ARRAY (NATURAL RANGE <>) OF INTEGER;
  FUNCTION func1(ia: integer_array := (1,2,3,next)) RETURN BOOLEAN IS
  BEGIN
    RETURN false;
  END FUNCTION func1;
BEGIN
END ARCHITECTURE arch;

package long_pkg is
end package;

package body long_pkg is
	procedure bar is
	begin
		report ""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ;                                                                                                                                                                                                                                                                                                                     natural ;
	end procedure;
end package body;
