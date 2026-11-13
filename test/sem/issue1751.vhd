package my_pkg is
    generic (
        type field_t
    );

    type rec_t is record
        a : field_t;
        b : field_t;
    end record;

    procedure set(constant c : rec_t; signal s : inout rec_t);
    function ident(x : field_t) return field_t;
end package;

package body my_pkg is
    procedure set(constant c : rec_t; signal s : inout rec_t) is
    begin
        s.a <= c.a;
        s.b <= c.b;
    end procedure;

    function ident(x : field_t) return field_t is
    begin
        return x;
    end function;
end package body;

entity issue1751 is
end entity;

architecture test of issue1751 is
    package inst is new work.my_pkg generic map (field_t => integer);

    alias rec_t is inst.rec_t;            -- OK

    alias set_bad is inst.set;            -- Error
    alias set_ok is inst.set [rec_t, rec_t];  -- OK
    alias ident_bad is inst.ident;        -- Error
    alias ident_ok is inst.ident [integer return integer];  -- OK

    alias eq_bad is "=";                  -- Error
    alias eq_ok is "=" [integer, integer return boolean];  -- OK

    type colour is (red, green, blue);
    alias red_bad is red;                 -- Error
    alias red_ok is red [return colour];  -- OK

    procedure local is
    begin
    end procedure;

    alias local_bad is local;             -- Error
    alias local_ok is local [];           -- OK

    function zero return integer is
    begin
        return 0;
    end function;

    alias zero_bad is zero;               -- Error
    alias zero_ok is zero [return integer];  -- OK

    signal rec : rec_t := (1, 2);
begin
    process is
    begin
        set_ok((a => 1, b => 2), rec);
        assert ident_ok(rec.a) = 1;
        assert eq_ok(zero_ok, 0);
        assert red_ok = red;
        local_ok;
        wait;
    end process;
end architecture;
