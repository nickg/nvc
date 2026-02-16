entity test_module is
  generic (my_generic : natural);
end test_module;

architecture rtl of test_module is
    signal my_sig : natural;
begin
  my_sig <= my_generic;
end rtl;
