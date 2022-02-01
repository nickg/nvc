entity e is end entity;

architecture a of e is
begin

    b: block is
    begin
    end block;

    c: block is
        signal x : integer;
        signal y : real;
    begin
        x <= integer(y);
    end block;

end architecture;
