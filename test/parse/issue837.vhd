entity issue837 is
    generic ( g : integer );
    port ( p : in bit_vector(1 to g - 1) );
end entity;

architecture test of issue837 is
    signal s : p'subtype;               -- OK
begin
end architecture;
