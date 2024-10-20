entity foo is end entity;
architecture arch of foo is
    type unsigned is array (natural range <>) of bit;
begin
  process is
    procedure xyzzy( v : inout unsigned ) is
    begin
    end procedure;

    variable x : bit_vector( 7 downto 0 );
  begin
    -- trying to do an inout conversion triggers the bug:
    xyzzy( bit_vector( v ) => unsigned( x ) );  -- Error
    wait;
  end process;
end architecture;
