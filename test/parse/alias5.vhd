package test_pkg is
  alias alias_t is type_t;
  alias alias_t is type_t;
  constant c : alias_t;
end package;

package pack is
  procedure proc;
  alias proc_alias is x;
  alias proc_alias is proc [integer];
end package;
