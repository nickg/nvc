entity issue917 is
end entity;

architecture test of issue917 is
    function add (x, y : integer) return integer is
    begin
        return x + y;
    end function;

    impure function impure_func (x : integer) return integer is
    begin
        return x + 1;
    end function;

begin

    b1: block is
        generic ( pure function f1 (x, y : integer) return integer );  -- OK
        generic map ( f1 => add );      -- OK
    begin
    end block;

    b2: block is
        generic ( impure function f1 (x : integer) return integer );  -- OK
        generic map ( f1 => impure_func );      -- OK
    begin
    end block;

    b3: block is
        generic ( function f1 (x : integer) return integer );  -- OK
        generic map ( f1 => impure_func );      -- Error
    begin
    end block;

    b4: block is
        generic (
            pure function f1 (x, y : integer) return integer is add;  -- OK
            impure function f1 (x : integer) return integer is add );  -- Error
    begin
    end block;

    b5: block is
        generic (
            pure function f1 (x, y : integer) return integer is <> );  -- Error
    begin
    end block;

end architecture;
