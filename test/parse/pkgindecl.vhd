entity pkg_in_decl is begin end entity;

architecture arch of pkg_in_decl is
  function test(
    package inner_pkg is end package; -- error
  ) is begin
  end function;
begin
end architecture;
