entity issue930 is
end entity;

architecture test of issue930 is

    signal name : string(1 to 40) := (others=>' ');
    signal s1, s2, s3, s4 : string(1 to 20);

begin

    p_nvc_bug : process

        procedure set_name(
            value : string
        ) is
        begin
            name(1 to value'length)  <= value;  -- Drives all of value
        end procedure set_name;

        procedure test2(value : string) is
        begin
            s1(1 to value'left) <= (others => ' ');
            s2(value'low to value'high) <= (others => ' ');
            s3(value'range) <= (others => ' ');
            s4(value'reverse_range) <= (others => ' ');
        end procedure;

    begin
        report "test done";
        wait;
    end process p_nvc_bug;

end architecture test;
