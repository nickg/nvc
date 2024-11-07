package timing_pkg is
  type type1 is range real'high;       -- error
  type type2 is range real'base'low;   -- error
  type type3 is range real'base'range; -- ok
  type type4 is range natural'range;   -- ok
end package;
