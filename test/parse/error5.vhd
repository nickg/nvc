-- Errors in context
use work.nothere.all;                   -- Error
package p is
    constant k : mytype := myfunc;      -- Error (suppressed)
end package;

package body p is
    constant y : mytype := myfunc;      -- Error (suppressed)
end package body;
