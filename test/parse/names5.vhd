package adder is
    generic (type t);
    function func return t;

    package nested is
        constant k : integer := 5;
    end package;
end package;

-------------------------------------------------------------------------------

entity sub is
    generic (
        package p is new work.adder generic map (<>) );
    port (
        x : in p.t;                     -- OK
        y : in p.tt;                    -- Error
        z : in p.func;                  -- Error
        a : in p.nested;                -- Error

        o : out p.t );
end entity;
