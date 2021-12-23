entity sub is
    generic ( n : natural );
end entity;

architecture test of sub is

    function get_n return integer is
    begin
        return n;
    end function;

    constant x : natural := get_n;

begin
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
begin

    sub_1: entity work.sub generic map ( 5 );
    sub_2: entity work.sub generic map ( 10 );

end architecture;
