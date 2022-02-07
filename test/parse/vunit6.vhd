-- library foo

package pack is
    function func(x : integer) return integer;
end package;

context foo_ctx is
    use foo.pack.all;
end context;

use work.pack.all;
context foo.foo_ctx;

entity ent is
    generic ( g : integer := func(2) );
end entity;
