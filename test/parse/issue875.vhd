package repro is
  subtype instruction32_t is bit_vector(31 downto 0);
  subtype opcode32_t is bit_vector(6 downto 0);

  function has_rd(op  : opcode32_t) return boolean;
  function has_rd(ins : instruction32_t) return boolean;
end;

package body repro is
  function has_rd(op : opcode32_t) return boolean is
  begin
    return false;
  end function;

  function has_rd(ins : instruction32_t) return boolean is
  begin
    return true;
  end function;
end package body;
