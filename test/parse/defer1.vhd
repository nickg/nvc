package defer1 is
    function func1 return integer;
    constant c1 : integer := func1;     -- Error
    constant c2 : integer;
    constant c3 : integer := c2 + 1;    -- Error
    function func2 (x : integer := c2) return integer;  -- OK
end package;

-------------------------------------------------------------------------------

entity e is
end entity;

architecture a of e is
    type pt1 is protected
    end protected;

    shared variable v1 : pt1;           -- Error

    type pt1 is protected body
    end protected body;
begin
end architecture;
