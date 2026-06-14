entity issue1576 is
end entity;

architecture test of issue1576 is

    type data_t is array (natural range <>) of bit_vector;
    type container_t is record
        data : data_t;
    end record;
    type container_ptr_t is access container_t;

begin

    process is
        variable container : container_ptr_t := null;
        variable size : natural := 0;

        procedure new_container is
            constant INITIAL_CAPACITY : positive := 4;
            subtype container_constr_t is container_t(data(0 to INITIAL_CAPACITY-1)(7 downto 0));
        begin
            container := new container_constr_t;
            container.data := (0 to INITIAL_CAPACITY-1 => (0 to 7 => '1'));
            size := 0;
        end procedure;

        procedure append_data (data : bit_vector(7 downto 0)) is
        begin
            container.data(size) := data;
            size := size + 1;
        end procedure;
    begin
        new_container;
        append_data(data => "00000001");
        append_data(data => "00000010");
        append_data(data => "00000100");
        append_data(data => "00001000");

        for k in 0 to size-1 loop
            report to_string(k);
            report to_string(container.data(k));
        end loop;

        assert container.data(0) = "00000001";
        assert container.data(1) = "00000010";
        assert container.data(2) = "00000100";
        assert container.data(3) = "00001000";

        wait;
    end process;

end architecture;
