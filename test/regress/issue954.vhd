entity issue954 is
end entity;

architecture test of issue954 is
type StringArray_t                  is array (natural range <>) of string;

    constant MyStringArray_c : StringArray_t(0 to 1) := (
        0 => "TEST 0",
        1 => "TEST 1"
    );
begin

    process is
    begin
        for i in 0 to 1 loop
            assert MyStringArray_c(i) = "TEST " & integer'image(i);
        end loop;
        wait;
    end process;

end architecture test;
