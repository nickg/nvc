library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pkg is
  type t is protected
    procedure f(k : string);
    procedure g(signal a : out std_logic_vector);
  end protected;
end package;

package body pkg is
  type t is protected body

    type char_type is file of character;
    file h : char_type;

    impure function d(n : integer) return string is
      variable s : string(1 to n);
    begin
      for i in 1 to n loop
        read(h, s(i));
      end loop;
      return s;
    end function;

    impure function d(n : integer) return std_logic_vector is
      constant s : string := d(n);
      variable v : std_logic_vector(n * 8 - 1 downto 0);
    begin
      for i in 1 to n loop
        v((i*8) - 1 downto (i-1) * 8) :=
          std_logic_vector(to_unsigned(character'pos(s(i)), 8));
      end loop;
      return v;
    end function;

    procedure f(k : string) is
      variable j : file_open_status;
    begin
      file_open(j, h, k, read_mode);
    end procedure;

    procedure g(signal a : out std_logic_vector) is
    begin
      a <= d(4);
    end procedure;

  end protected body;
end package body;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

entity issue675 is
end entity;

architecture sim of issue675 is
  shared variable a : t;
  signal c : std_logic_vector(31 downto 0);
begin
  process
    procedure d(b : string) is
    begin
      a.f(b);
      a.g(c);
    end procedure;

    type char_type is file of character;
    file h : char_type;
  begin
    file_open(h, "tmp.bin", write_mode);
    write(h, '2');
    write(h, 'F');
    write(h, '2');
    write(h, 'F');
    file_close(h);

    d("tmp.bin");
    wait for 0 ns;
    assert c = X"46324632";
    wait;
  end process;

end architecture;
