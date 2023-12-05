package PassthroughDUTPkg is

    type VideoDataArray is array (natural range <>) of bit_vector;

end PassthroughDUTPkg;
-------------------------------------------------------------------------------

use work.PassthroughDUTPkg.all;

entity PassthroughDUT is
    generic (
        PIXEL_DEPTH      : natural := 24; -- number of bits in a pixel
        NUM_DATA_STREAMS : natural := 1   -- number of pixel data streams
    );
    port (
        -- VideoBus input
        vid_fval_in    : in  bit;
        vid_lval_in    : in  bit;
        vid_dval_in    : in  bit;
        vid_data_in    : in  VideoDataArray(0 to NUM_DATA_STREAMS-1)(PIXEL_DEPTH-1 downto 0);
        -- VideoBus output
        vid_fval_out   : out bit;
        vid_lval_out   : out bit;
        vid_dval_out   : out bit;
        vid_data_out   : out VideoDataArray(0 to NUM_DATA_STREAMS-1)(PIXEL_DEPTH-1 downto 0)
    );
end entity;

architecture SimplePassthrough of PassthroughDUT is
begin
    vid_fval_out <= vid_fval_in;
    vid_lval_out <= vid_lval_in;
    vid_dval_out <= vid_dval_in;
    vid_data_out <= vid_data_in;
end architecture;

-------------------------------------------------------------------------------

entity elab40 is
    generic (
        PIXEL_DEPTH      : natural := 24;  -- number of bits in a pixel (RGB = 3*8)
        NUM_DATA_STREAMS : natural := 1    -- number of pixel data streams
    );
end entity;

architecture test of elab40 is
    --- VideoBus input
    signal vid_fval_in    : bit;
    signal vid_lval_in    : bit;
    signal vid_dval_in    : bit;
    signal vid_data_in    : bit_vector(PIXEL_DEPTH-1 downto 0);
    -- VideoBus output
    signal vid_fval_out   : bit;
    signal vid_lval_out   : bit;
    signal vid_dval_out   : bit;
    signal vid_data_out   : bit_vector(PIXEL_DEPTH-1 downto 0);

begin

    passthroughdut_inst: entity work.PassthroughDUT
    generic map (
        PIXEL_DEPTH      => PIXEL_DEPTH,
        NUM_DATA_STREAMS => NUM_DATA_STREAMS
    )
    port map (
        vid_fval_in     => vid_fval_in,
        vid_lval_in     => vid_lval_in,
        vid_dval_in     => vid_dval_in,
        vid_data_in(0)  => vid_data_in,
        vid_fval_out    => vid_fval_out,
        vid_lval_out    => vid_lval_out,
        vid_dval_out    => vid_dval_out,
        vid_data_out(0) => vid_data_out
    );

    check: process is
    begin
        vid_data_in <= X"123456";
        wait for 1 ns;
        assert vid_data_out = X"123456";
        wait;
    end process;

end architecture;
