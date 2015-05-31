entity e is
end entity;

architecture a of e is
    constant x : integer := 5;

    function f_pure(n : in integer) return integer is
    begin
        return n + 1;
    end function;

    impure function f_impure return integer is
    begin
        return x;
    end function;

    signal s : integer;
begin

    process is
        variable v : integer;
    begin
        v := x;                         -- OK
        x := v;                         -- Error
    end process;

    process is
        constant c : integer;           -- Error
    begin
    end process;

    process is
        constant c : integer := f_pure(5);  -- OK
    begin
    end process;

    process is
        constant c : integer := f_impure;      -- OK (LRM 93 7.4.2 note 2)
    begin
    end process;

end architecture;

-------------------------------------------------------------------------------

package p is

    constant c : integer;               -- OK
    constant d : integer;
    constant e : integer := c + 1;      -- OK
    constant f : integer;

end package;

package body p is

    constant c : integer := 6;          -- OK
    constant c : integer := 6;          -- Error
    constant f : bit := '1';            -- Error

    -- Missing definition for d
end package body;
