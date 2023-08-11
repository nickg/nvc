package error7 is
    type foo is (a, b, c);
    function "=" (l, r : foo) return boolean;
    constant foo : integer := 1;        -- Error
end package;

package body error7 is                  -- Error
    constant k : boolean := a = b;      -- Error (suppressed)
end package body;

use work.error7.all;                    -- Error

package other is
    constant x : integer := bad;        -- Error (suppressed)
end package;

use work;                           -- Error
