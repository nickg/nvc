-- library foo
entity foo_e is
end entity;

architecture test of foo_e is
begin
end architecture;

-------------------------------------------------------------------------------
-- library bar

library foo;

configuration foo_cfg of foo.foo_e is
    for test
    end for;
end configuration;

