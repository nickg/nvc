-- Test that an implicit signal attribute (e.g. 'delayed) can be used
-- inside each branch of an if/elsif/else generate statement.  The
-- implicit signal created for the attribute is declared in the scope
-- of the enclosing generate branch, which must be a valid declarative
-- region (the T_COND_STMT for that branch, not the T_IF_GENERATE as
-- a whole).  Three separate generate statements below exercise the
-- "if", "elsif" and "else" branches respectively.

entity implicit11 is
end entity;

architecture test of implicit11 is
  signal a          : bit := '0';
  signal b, c, d, e : bit;
begin

  gen_if : if true generate
    b <= a and not a'delayed(2 ns);
  end generate gen_if;

  gen_elsif : if false generate
    e <= '0';
  elsif true generate
    c <= a and not a'delayed(2 ns);
  end generate gen_elsif;

  gen_else : if false generate
    e <= '0';
  else generate
    d <= a'delayed(2 ns);
  end generate gen_else;

  process is
  begin
    wait for 1 ns;
    a <= '1';
    wait for 1 ns;
    assert b = '1' report "expected b = 1 (if branch)" severity failure;
    assert c = '1' report "expected c = 1 (elsif branch)" severity failure;
    assert d = '0' report "expected d = 0 (else branch)" severity failure;
    report "PASSED";
    wait;
  end process;
end architecture;
