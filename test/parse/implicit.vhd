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

--------------------------------------------------------------------------------

entity e is begin end entity;

architecture a of e is
    signal s : integer;
begin
    process is begin
        assert s'delayed(undefined) = 5;  -- Error
        assert s'delayed(undefined) = 5;  -- Error (suppressed)
        wait;
    end process;
end architecture;
