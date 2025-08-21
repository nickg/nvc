package pkg_test is

  type test_t is record
    x : bit_vector(7 downto 0); -- Only vector types cause the issue.
  end record test_t;

  constant c1 : integer range 1 to 3;

end package pkg_test;

use work.pkg_test.all;

entity bug is
end entity bug;

architecture tb of bug is

  signal test_signal : test_t;

  subtype test_type is test_signal.x'subtype; -- Offending line.

  subtype sub2 is c1'subtype;
begin

end architecture tb;
