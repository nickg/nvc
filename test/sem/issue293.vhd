entity test is
end test;

architecture behv of test is

  -- Crash "** Fatal: tree kind T_TYPE_CONV does not have item I_ATTRS"
  constant AWIDTH : integer := integer(4);  -- or natural
  signal a : bit_vector (AWIDTH downto 0);

begin

end behv;
