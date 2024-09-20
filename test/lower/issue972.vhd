entity issue972 is
end entity;

architecture test of issue972 is
    type t_rec is record
        f : integer;
    end record;
begin

    b: block is
        generic ( r : t_rec );
        generic map ( r => (f => 5) );
        signal s : integer range 1 to r.f;
    begin
    end block;

end architecture;
