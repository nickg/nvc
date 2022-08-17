entity implicit is
end entity;

architecture test of implicit is
    signal x : integer;
begin

    process is
    begin
        assert x'delayed = 4;           -- OK
        assert x'delayed(1 ns) = 5;     -- OK
        assert x'delayed(5) = 1;        -- Error
        assert x'stable;                -- OK
        assert x'stable(1 ns);          -- OK
        assert x'delayed'stable(2 ns);  -- OK
        assert x'transaction = '1';     -- OK
        assert x'quiet;                 -- OK
        assert x'quiet(5 ns);           -- OK
    end process;

end architecture;
