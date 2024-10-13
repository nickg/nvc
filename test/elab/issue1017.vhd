entity sub is
    generic (type t is array (natural range <>) of type is private);
end entity;

architecture test of sub is
    type data_array is array (natural range <>, natural range <>) of t'element ;
    signal data : data_array(1 to 2, 2 to 3) ;
begin
end architecture;

-------------------------------------------------------------------------------

entity issue1017 is
end entity;

architecture test of issue1017 is
begin
    u: entity work.sub generic map (integer_vector);
end architecture;
