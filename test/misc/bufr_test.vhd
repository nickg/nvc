library ieee;
use ieee.std_logic_1164.all;

library unisim;
use unisim.vcomponents.all;

entity bufr_test is
end entity;

architecture test of bufr_test is
    component BUFR
        generic ( BUFR_DIVIDE : string := "BYPASS";
                  SIM_DEVICE  : string := "7SERIES");
        port (    O           : out STD_LOGIC;
                  CE          : in  STD_LOGIC;
                  CLR         : in  STD_LOGIC;
                  I           : in  STD_LOGIC);
    end component;
    attribute BOX_TYPE of BUFR : component is "PRIMITIVE";

    signal amu_adc_dco_i : std_logic;
    signal amu_adc_dco   : std_logic;
begin

    BUF_DATA_CLK : BUFR
        generic map (  BUFR_DIVIDE    => "BYPASS",
                       SIM_DEVICE     => "7SERIES")
        port map (  O              => amu_adc_dco,
                    CE             => '1',
                    CLR            => '0',
                    I              => amu_adc_dco_i);

end architecture;
