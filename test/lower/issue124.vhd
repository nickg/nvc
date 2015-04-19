package pack is
  type unsigned is array (integer range <>) of bit;

  function to_integer (constant value : unsigned) return integer;
end package;

entity to_integer_string_bug is
end entity;

use work.pack.all;

architecture test of to_integer_string_bug is
  function to_integer_string (constant value : unsigned) return string is
  begin
      return integer'image(to_integer(value));  -- 'IMAGE is safe in return context
  end;
begin
end architecture;
