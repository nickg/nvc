-- Library foo
package foo is
    constant c : integer := 1;
end package;

-- Library bar
library foo;
use foo.foo.all;
package barpack is
    constant c : integer := 2;
end package;

-- Library baz
library foo;
use foo.all;
library bar;
use bar.all;
use bar.*;                              -- Error
package bazpack is
    constant c : integer := foo.foo.c + barpack.c;
end package;
