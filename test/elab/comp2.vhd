entity sub1 is
    generic ( x : integer := 4 );
end entity;

architecture test of sub1 is
    constant y : integer := x;
begin
end architecture;

-------------------------------------------------------------------------------

entity sub2 is
    generic ( x : integer := 5 ;
              y : boolean := true );
end entity;

architecture test of sub2 is
begin
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
    component sub1 is
    end component;

    component sub2 is
        generic ( y : boolean := false ;
                  x : integer := 7 );
    end component;
begin

    sub1_i: component sub1;

    sub2_i: component sub2;

end architecture;
