entity pragma_test is
end pragma_test;

architecture dummy of pragma_test is  -- OK
begin
  synerror: assert false
-- pragma translate_off
     or true
-- pragma translate_on
     report "dummy model found during synthesis" severity failure;

end architecture dummy;


architecture dummy2 of pragma_test is  -- generates parsing error
begin
  synerror: assert false
    -- pragma translate_off
     or true
    -- pragma translate_on
     report "dummy model found during synthesis" severity failure;

end architecture dummy2;
