package COMPONENTS is
    component DUMMY_MODULE
        port (I : in bit; O : out bit);
    end component;
end package;

entity DUMMY_MODULE is
    port (I: in bit; O: out bit);
end entity;
architecture RTL of DUMMY_MODULE is
begin
    O <= I;
end architecture;

use     WORK.COMPONENTS.DUMMY_MODULE;
entity  DUMMY_TOP is
    port (I : in bit; O : out bit);
end entity;
architecture RTL of DUMMY_TOP is
begin
    U: DUMMY_MODULE port map(I=>I, O=>O);
end architecture;
