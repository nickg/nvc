entity test_mod is

  generic (
    size : integer
  );

end entity test_mod;

architecture test of test_mod is
  signal x : bit;
begin
  assert size mod 4 = 0;        -- OK
  assert (size mod 4) = 0;      -- OK
  assert (size+1) mod 4 = 0;    -- OK
  assert (size mod 4 = 0);      -- OK
  assert (size + 1) rem 2 = 0;  -- OK
  assert (size + 1) + 2 = 0;    -- OK
  assert (size + 1) - 2 = 0;    -- OK
  assert (size + 1) * 2 = 0;    -- OK
  assert (size + 1) / 2 = 0;    -- OK
  assert (size + 1) ** 2 = 0;   -- OK
  assert (x or '1') ?= '0';     -- OK
  assert (x or '1') ?/= '0';    -- OK
  assert (x or '1') ?<= '0';    -- OK
  assert (x or '1') ?> '0';     -- OK
end test;
