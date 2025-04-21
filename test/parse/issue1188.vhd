package pkg is

  function f_gen(
    use_id         : boolean;
    id             : bit_vector(15 downto 0) := x"1234"
  ) return bit_vector;

end pkg;

package body pkg is

  constant C_ID           : bit_vector(15 downto 0) := x"0110";

  function f_if(c : boolean; t : bit; f : bit) return bit is
    variable r : bit;
  begin
    r := t when c else f;
    return r;
  end;

  function f_if(c : boolean; t : bit_vector; f : bit_vector) return bit_vector is
    variable r : bit_vector(t'range);
  begin
    r := t when c else f;
    return r;
  end;

  function f_gen(
    use_id         : boolean;
    id             : bit_vector(15 downto 0) := x"1234"
  ) return bit_vector is

    variable ret           : bit_vector(31 downto 0);
  begin
    ret := (
      31 downto 16 => C_ID,
      15 downto 0  => f_if(use_id, t=> id, f=> C_ID)  -- OK
    );
    return ret;
  end;

end package body pkg;
