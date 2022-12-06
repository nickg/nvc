entity signal5 is
end entity;

architecture test of signal5 is
    signal s : bit;
begin

    p1: process is
    begin
        -- Should only have one drive signal call
        s <= '1';
        s <= '0';
        s <= '1';
        wait;
    end process;

end architecture;
