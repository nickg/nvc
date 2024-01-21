entity bist_RF1 is
   generic ( WIDTH : natural range 1 to 32 );
   port (
   D  : in  bit_vector(WIDTH-1 downto 0));
begin
end bist_RF1;

architecture bist of bist_RF1 is
begin
end bist;

configuration bist_rf1_inferred of bist_RF1 is
  for bist
  end for;
end configuration;

-------------------------------------------------------------------------------

entity gpsif_regfile is
end gpsif_regfile;
architecture beh of gpsif_regfile is

component bist_RF1
   generic ( WIDTH : natural range 1 to 32 );
   port ( D  : in  bit_vector(WIDTH-1 downto 0) );
end component;

signal x : bit_vector(19 downto 0);
signal y : bit_vector(29 downto 0);
begin
    sftca_bank : bist_RF1
    generic map ( WIDTH => 20 )
    port map
     (D => x);                          -- OK
    pnco_bank : bist_RF1
    generic map ( WIDTH => 30 )
    port map
     ( D => y);                         -- OK
end beh;

-------------------------------------------------------------------------------

entity gpsif is
end gpsif;
architecture beh of gpsif is
component gpsif_regfile is
end component;

begin
  regfile : gpsif_regfile;

end beh;

-------------------------------------------------------------------------------

entity gpsif_top is
end entity;

architecture arch of gpsif_top is
    component gpsif is
    end component;
begin
    g : gpsif;
end architecture;

-------------------------------------------------------------------------------

configuration gpsif_sim of gpsif is
  for beh
    for regfile : gpsif_regfile
      use entity work.gpsif_regfile(beh);
      for beh
        for all : bist_RF1
          use configuration work.bist_rf1_inferred;
        end for;
      end for;
    end for;
  end for;
end configuration;

configuration gpsif_top_sim of gpsif_top is
  for arch
    for g : gpsif
      use configuration work.gpsif_sim;
    end for;
  end for;
end configuration;
