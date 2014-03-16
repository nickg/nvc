entity c is
    port (i : in bit);
begin
    assert (i = '0') report "not '0'" severity note;  -- OK
    assert (i = '1') report "not '1'" severity note;  -- OK
end entity c;
