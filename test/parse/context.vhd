context widget_context is
    library ieee;
    use ieee.std_logic_1164.all, ieee.numeric_std.all;
    use widget_lib.widget_defs.all;
    use widget_lib.widget_comps.all;
end context;

context dongle_context is
    library widget_lib;
    context widget_lib.widget_context;
end context;

library foo;
use foo.moo;

context bad is                          -- Error
end context;
