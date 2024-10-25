entity crash is
end entity;

architecture rtl of crash is
    signal crash_signal : bit_vector(31 downto 0);
begin
    crash_signal(11 downto 0) <= crash_signal;
end architecture;
