-- Library foo
package pack is
    type my_int is range 1 to 10;
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
    port (
        x : in my_int );
end entity;

-------------------------------------------------------------------------------

context foo.pack;                       -- Error

entity e2 is
end entity;

-------------------------------------------------------------------------------

package pack2 is
end package;

-------------------------------------------------------------------------------

context work_context is
    library work;                       -- Error
    use work.pack2;                     -- Error
    context work.blah;                  -- Error
end context;

-------------------------------------------------------------------------------

context nested is
    library foo;
    context foo.test_context;
end nested;

context work.nested;

entity foo is
    port (
        x : in my_int );                -- OK
end entity;
