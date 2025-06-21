entity issue1218 is
end entity;

library nvc;
use nvc.cover_pkg.all;

architecture test of issue1218 is
begin

    -- These should be no-ops when coverage not enabled
    process is
        variable scope : t_scope_handle;
        variable item : t_item_handle;
    begin
        create_cover_scope(scope, "foo");
        set_cover_scope_name(scope, "bar");
        add_cover_item(scope, item, "item", 0, ( (1 to 0 => (others => 0))));
        increment_cover_item(scope, item);
        wait;
    end process;

end architecture;
