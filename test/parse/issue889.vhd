entity issue889 is
end entity;

architecture test of issue889 is
    signal x, y : bit;

    attribute mark_debug : string;
    attribute mark_debug of all : signal is "True";  -- OK
begin

    process is
    begin
        report x'mark_debug;            -- OK
        wait;
    end process;

end architecture;
