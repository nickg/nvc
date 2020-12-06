-- Library foo
package pack is
end package;

-------------------------------------------------------------------------------

-- Library bar
context test_context is
    library foo;
    use foo.pack.all;
end context;

-------------------------------------------------------------------------------

-- Library bar
library foo;
context foo.test_context;

entity foo is
end entity;
