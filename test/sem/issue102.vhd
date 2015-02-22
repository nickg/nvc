package COMPONENTS is
    component DUMMY_MODULE
        port (I : in bit; O : out bit);
    end component;
end package;

use     WORK.COMPONENTS.DUMMY_MODULE;
entity  DUMMY_TOP is
    port (I : in bit; O : out bit);
end entity;
architecture RTL of DUMMY_TOP is
begin
    U: DUMMY_MODULE port map(I=>I, O=>O);
end architecture;
