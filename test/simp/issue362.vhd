entity bug is
end entity;

architecture a of bug is
  type enum_t is (enum0, enum1);
  subtype enum_subtype_t is enum_t;

  type enum_vec_t is array (natural range <>) of enum_t;
  function resolve_enum(values : enum_vec_t) return enum_t;
  subtype resolved_enum_t is resolve_enum enum_t;

  function resolve_enum(values : enum_vec_t) return enum_t is
  begin
    return values(0);
  end;

  function func return enum_t is
  begin
    return enum_subtype_t'low;
  end;

  function func2 return enum_t is
  begin
    return resolved_enum_t'high;
  end;

begin

  main : process is
  begin
    -- Should both be optimised out during constant folding
    assert func = enum_t'low;
    assert func2 = enum_t'high;
    wait;
  end process;
end;
