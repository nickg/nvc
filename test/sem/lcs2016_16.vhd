entity ent1 is
    generic ( g1 : type is private );   -- OK
    port ( p1 : type is private );      -- OK
end entity;
