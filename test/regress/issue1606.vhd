-- Package ---------------------------------------------------------------------
package crash_pkg is

  type word_vec_t is array (natural range <>) of bit_vector(0 downto 0);
  type bool_vec_t is array (natural range <>) of boolean;

end package;


-- Entity ----------------------------------------------------------------------
use work.crash_pkg.all;

entity crash is
  generic (
    G_ENABLES  : bool_vec_t;
    G_VALUES   : word_vec_t(G_ENABLES'range) := (others => (others => '0'))
  );
  port (
    resp   : out bit;
    values : out word_vec_t
  );
end entity;

architecture rtl of crash is
begin

  resp <= '1';
  values <= G_VALUES;

end architecture;


-- Testbench -------------------------------------------------------------------
use work.crash_pkg.all;

entity issue1606 is
end entity;

architecture tb of issue1606 is

  constant ENABLES0 : bool_vec_t(0 to 1) := (false, true);
  constant ENABLES1 : bool_vec_t(0 to 2) := (true, true, false);

  constant VALUES0 : word_vec_t(0 to 1) := ("1", "0");
  constant VALUES1 : word_vec_t(0 to 2) := (others => (others => '0'));

  signal rsp0 : bit;
  signal rsp1 : bit;
  signal actual0 : word_vec_t(0 to 1);
  signal actual1 : word_vec_t(0 to 2);

begin

  -- One instance alone does not crash. The second instance is needed.
  u_first : entity work.crash
  generic map (
    G_ENABLES  => ENABLES0,
    -- If you comment out this line so that BOTH instances do not have
    -- explicitly set G_VALUES generics then the sim does NOT crash.
    G_VALUES   => VALUES0
  )
  port map (
    resp   => rsp0,
    values => actual0
  );

  -- This second instance WITHOUT the G_VALUES generic explicitly set causes
  -- a crash
  u_second : entity work.crash
  generic map (
    G_ENABLES => ENABLES1
    -- If you uncomment this line so that both instances DO explicitly set
    -- G_VALUES, then there is no crash. The crash only happens when one
    -- instance sets it and the other does not.
    -- G_VALUES   => VALUES1
  )
  port map (
    resp   => rsp1,
    values => actual1
  );

  check : process is
  begin
    wait for 0 ns;
    assert actual0 = VALUES0;
    assert actual1 = VALUES1;
    wait;
  end process;

end architecture;
