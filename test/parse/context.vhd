package widget_defs is
end package;

package widget_comps is
end package;

context widget_context is
    use widget_lib.widget_defs.all;
    use widget_lib.widget_comps.all;
end context;

-------------------------------------------------------------------------------

context dongle_context is
    library widget_lib;
    context widget_lib.widget_context;
end context;

library widget_lib;
use widget_lib.widget_defs;

context bad is                          -- Error
end context;
