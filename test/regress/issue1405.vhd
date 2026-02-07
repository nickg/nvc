entity sub is
    generic (g : integer);
end entity;

architecture test of sub is
    constant c : integer_vector(1 to 6) := (others => g);

    type pt is protected
        impure function get return integer;
    end protected;

    type pt is protected body
        impure function get return integer is
        begin
            return c(1);
        end function;
    end protected body;

    shared variable v : pt;
    constant k : integer := v.get;
begin
end architecture;

-------------------------------------------------------------------------------

entity issue1405 is
end entity;

architecture test of issue1405 is
    constant g : integer := 42;
begin

    u1: entity work.sub
        generic map (g);

    u2: entity work.sub
        generic map (g);

end architecture;
