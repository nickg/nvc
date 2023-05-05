Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity parse5 is
end entity;

architecture test of parse5 is

    signal a, b, c, d, e, f, clk : bit;

begin

    -- psl default clock is rising_edge(clk);

    -- psl cover {a:b:c:d};

    -- psl cover {a;b:c};

    -- psl cover {{a}|{b}};
    -- psl cover {a[*5]|b[+]};
    -- psl cover {{a;b}|{c;d}|{d;a}};

    -- psl cover {{a;b} within {c;d;e;f}};

    -- psl cover {{a;b;c} && {d;e;f}};

    -- psl cover {{a;b;c} & {d;e}};

    -- psl cover {{a:b} & {c;d} && {e;f} | {d;e}};

end architecture;
