entity bounds2 is
    generic (
        PIXEL_DEPTH      : natural := 24 );
end entity;

architecture test of bounds2 is
    signal vid_data_in : bit_vector(PIXEL_DEPTH-1 downto 0);
begin
    p: process is
    begin
        vid_data_in <= X"123";          -- Error
        wait;
    end process;
end architecture;
