-- Test that a named port association can select a nested record field
-- (e.g. "p.b.x => sig") without corrupting a sibling association that
-- selects only one level of the same port (e.g. "p.a => sig"). The
-- "direct mapping" optimisation in lower_direct_mapped_port() only
-- unwrapped one level of T_RECORD_REF, so it silently skipped anything
-- deeper without marking the port as unsafe to direct-map. A sibling
-- single-level association on the same port was then direct-mapped
-- anyway, leaving the port partially aliased and corrupting its memory
-- layout, which crashed the JIT-compiled model at elaboration time.

package portagg2_pkg is
  type inner_t is record
    x : integer;
    y : integer;
  end record;
  type outer_t is record
    a : integer;
    b : inner_t;
  end record;
end package;

-------------------------------------------------------------------------------

use work.portagg2_pkg.all;

entity portagg2_sub is
  port (
    p : in  outer_t;
    o : out integer
  );
end entity;

architecture rtl of portagg2_sub is
begin
  o <= p.a + p.b.x + p.b.y;
end architecture;

-------------------------------------------------------------------------------

use work.portagg2_pkg.all;

entity portagg2 is
end entity;

architecture test of portagg2 is
  signal sig_a  : integer := 1;
  signal sig_bx : integer := 2;
  signal sig_by : integer := 3;
  signal result : integer;
begin
  uut : entity work.portagg2_sub
    port map (
      p.a   => sig_a,
      p.b.x => sig_bx,
      p.b.y => sig_by,
      o     => result
    );

  process is
  begin
    wait for 1 ns;
    assert result = 6
      report "expected 6, got " & integer'image(result)
      severity failure;

    sig_a <= 10;
    wait for 1 ns;
    assert result = 15
      report "expected 15 after update, got " & integer'image(result)
      severity failure;

    report "PASSED";
    wait;
  end process;
end architecture;
