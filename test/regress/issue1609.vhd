entity issue1609 is end entity;

architecture rtl of issue1609 is

    subtype t_index is natural range 0 to 1;

    type t_target is record
        my_bit : bit;
    end record;

    type t_target_array is array(t_index) of t_target;
    signal target : t_target_array;

    signal idx : t_index;

begin

    process(all)
    begin
        target <= (others => (my_bit => '0'));
        target(idx).my_bit <= '1';
    end process;

    process
    begin
        idx <= 0;
        wait for 10 ns;
        idx <= 1;
        wait for 10 ns;
        idx <= 0;
        wait for 10 ns;

        std.env.finish;
    end process;

    postponed process
    begin
        wait until idx'event;
        assert target(idx).my_bit = '1'
            report "At idx " & to_string(idx) & " bit is " & to_string(target(idx).my_bit)
            severity failure;
        wait;
    end process;

end architecture rtl;
