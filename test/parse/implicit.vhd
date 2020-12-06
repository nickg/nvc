package implicit is
    type unsigned is array (natural range <>) of bit;
    function ">="(x, y : unsigned) return boolean;
end package;

package body implicit is

    procedure test is
        variable a, b : unsigned(1 to 5);
    begin
        assert a >= b;                  -- OK
    end procedure;

end package body;
