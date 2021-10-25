package pack is
    constant x : integer := 10;
end package;

entity e is
end entity;

architecture a of e is

    procedure test1 is
        constant x : integer := 5;
        use work.pack.x;                -- OK (work.pack.x is potentially visible and hidden
                                        -- by local x above)
    begin
        assert x = 5;                   -- OK
    end procedure;

    procedure test2 is
        use work.pack.x;
        constant x : integer := 5;
    begin
        assert x = 5;                   -- OK
    end procedure;

begin

end architecture;
