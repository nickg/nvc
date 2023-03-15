package mux is
  generic (
    MUX_DATA_SIZE : natural
  );
  subtype mux_data is bit_vector(MUX_DATA_SIZE-1 downto 0);
  type mux_data_array is array (natural range <>) of mux_data;
end package;

entity multiplexer is
  generic (
    MUX_DATA_SIZE : natural;
    MUX_CTRL_SIZE : natural;
    --package mux_g is new work.mux generic map(MUX_DATA_SIZE => MUX_DATA_SIZE)
    package mux_g is new work.mux generic map(<>)
  );
  port (
    MUX_CTRL : in  bit_vector(MUX_CTRL_SIZE-1 downto 0);
    MUX_IN   : in  mux_g.mux_data_array(0 to 2**MUX_CTRL_SIZE-1);
    MUX_OUT  : out mux_g.mux_data
  );
end entity;

architecture multiplexer_arch of multiplexer is
begin
  MUX_OUT <= MUX_IN(MUX_CTRL_SIZE - 1);
end architecture;

entity issue644 is
end entity ;

architecture arch of issue644 is

    constant MUX_DATA_SIZE : natural := 2 ;
    constant MUX_CTRL_SIZE : natural := 2 ;
    package mux_g is new work.mux generic map(MUX_DATA_SIZE => MUX_DATA_SIZE) ;

    signal MUX_CTRL : bit_vector(MUX_CTRL_SIZE-1 downto 0) ;
    signal MUX_IN   : mux_g.mux_data_array(0 to 2**MUX_CTRL_SIZE-1) ;
    signal MUX_OUT  : mux_g.mux_data ;

begin
    U_multiplexer : entity work.multiplexer
      generic map (
        MUX_DATA_SIZE => MUX_DATA_SIZE,
        MUX_CTRL_SIZE => MUX_CTRL_SIZE,
        mux_g         => mux_g
      ) port map (
        MUX_CTRL => MUX_CTRL,
        MUX_IN   => MUX_IN,
        MUX_OUT  => MUX_OUT
      ) ;
end architecture ;
