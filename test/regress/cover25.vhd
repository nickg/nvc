entity sub is
    generic ( count : natural );
end entity;

library nvc;
use nvc.cover_pkg.all;

architecture test of sub is
begin

    p1: process is
        variable handle : t_scope_handle;
        variable item : t_item_handle;
    begin
        create_cover_scope(handle, "sub_scope");
        add_cover_item(handle, item, "item");
        wait for 5 ns;
        for i in 1 to count loop
            increment_cover_item(handle, item);
        end loop;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity cover25 is
end entity;

library nvc;
use nvc.cover_pkg.all;

architecture test of cover25 is
    component sub5 is
    end component;

    for all : sub5 use entity work.sub generic map (5);
begin

    sub1_i: entity work.sub generic map (10);

    sub2_i: component sub5;

    p2: process is
        variable handle : t_scope_handle;
        variable item1, item2 : t_item_handle;
    begin
        create_cover_scope(handle, "top_scope");
        add_cover_item(handle, item1, "item1");
        add_cover_item(handle, item2, "item2");
        wait for 1 ns;
        increment_cover_item(handle, item1);
        increment_cover_item(handle, item2);
        wait for 1 ns;
        increment_cover_item(handle, item2);
        wait;
    end process;

    p3: process is
        variable handle : t_scope_handle;
        variable item1, item2 : t_item_handle;
    begin
        create_cover_scope(handle, "top_scope");
        for i in 1 to 3 loop
            add_cover_item(handle, item1, "dup");
        end loop;
        add_cover_item(handle, item2, " **.x~");
        wait;
    end process;

end architecture;
