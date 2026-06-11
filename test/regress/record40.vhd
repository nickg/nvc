entity record40 is
end entity;

architecture test of record40 is

    type data_t is array (natural range <>) of integer;
    type container_t is record
        data : data_t;
    end record;
    type container_ptr_t is access container_t;

begin

    process is
        variable container : container_ptr_t := null;
        variable size : natural := 0;

        procedure new_container is
            subtype container_constr_t is container_t(data(0 to 3));
        begin
            container := new container_constr_t;
            container.data := (0 to 3 => 0);
            size := 0;
        end procedure;

        procedure append_data (data : integer) is
        begin
            container.data(size) := data;
            size := size + 1;
        end procedure;
    begin
        new_container;
        append_data(10);
        append_data(20);
        append_data(30);
        append_data(40);

        for k in 0 to size-1 loop
            report integer'image(container.data(k));
        end loop;

        assert container.data = (10, 20, 30, 40) severity failure;

        wait;
    end process;

end architecture;
