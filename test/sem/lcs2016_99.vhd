entity lcs2016_99 is
end entity;

architecture test of lcs2016_99 is
begin

    process is
    begin
        assert natural'range'value = (0, natural'right, ascending);  -- OK
        assert natural'range'value = false;  -- Error
        assert natural'reverse_range = 4;  -- Error
        assert natural'reverse_range'value.direction = ascending;  -- OK
        assert xxxx'range'value = (0, 0, ascending);  -- Error
        wait;
    end process;

    process is
        variable v : bit_vector(1 to 3);
    begin
        assert v'range'value = (0, 0, ascending);  -- Error
        wait;
    end process;

end architecture;
