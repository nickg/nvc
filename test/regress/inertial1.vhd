-- Test that a non-static port actual of a non-homogeneous array type
-- (i.e. an array of records) can be used with an "others" aggregate.
-- This requires the inertial actual process created for the implicit
-- driver (LRM 6.5.6.3) to copy the whole array via a pointer rather
-- than treating it as a scalar load/store.

package inertial1_pkg is
  type rec_t is record
    x : integer;
    y : bit_vector(3 downto 0);
  end record;
  type rec_array_t is array (natural range <>) of rec_t;
end package;

-------------------------------------------------------------------------------

use work.inertial1_pkg.all;

entity inertial1_sub is
  generic ( n : natural := 4 );
  port (
    i : in  rec_array_t(0 to n - 1);
    o : out rec_t
  );
end entity;

architecture rtl of inertial1_sub is
begin
  o <= i(0);
end architecture;

-------------------------------------------------------------------------------

use work.inertial1_pkg.all;

entity inertial1 is
end entity;

architecture test of inertial1 is
  signal drive : rec_t := (x => 42, y => "1010");
  signal result : rec_t;
begin
  uut : entity work.inertial1_sub
    generic map ( n => 4 )
    port map (
      i => (others => drive),
      o => result
    );

  process is
  begin
    wait for 1 ns;
    assert result.x = 42
      report "expected x = 42, got " & integer'image(result.x)
      severity failure;
    assert result.y = "1010"
      report "expected y = 1010" severity failure;

    drive.x <= 7;
    wait for 1 ns;
    assert result.x = 7
      report "expected x = 7 after update, got " & integer'image(result.x)
      severity failure;

    report "PASSED";
    wait;
  end process;
end architecture;
