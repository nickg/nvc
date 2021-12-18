--
-- Grab bag of miscellaneous VHDL-2008 syntax
--

entity vhdl2008 is
end entity;

architecture test of vhdl2008 is

    type my_utype is (a, b, c);
    type my_utype_vector is array (natural range <>) of my_utype;

    function resolved (s : my_utype_vector) return my_utype;

    subtype my_type is resolved my_utype;
    subtype my_type_vector is (resolved) my_utype_vector;  -- OK

    type my_logical_vec is array (natural range <>) of bit;
begin

    process is
        variable b : bit;
        variable v : my_logical_vec(1 to 3);
    begin
        b := or v;                      -- OK
        if or v = '1' then end if;      -- OK
        b := and v;                     -- OK
        b := xor v;                     -- OK
        b := xnor v;                    -- OK
        b := nand v;                    -- OK
        b := nor v;                     -- OK
    end process;

    process is
        variable b : bit;
        variable v : my_logical_vec(1 to 3);
    begin
        b := b ?= '1';                  -- OK
        b := b ?/= '1';                 -- OK
        b := b ?< '0';                  -- OK
        b := b ?> '0';                  -- OK
        b := b ?<= '1';                 -- OK
        b := b ?>= '1';                 -- OK
        b := v ?= "101";                -- OK
        b := v ?/= "111";               -- OK
    end process;

    process is
        variable b : bit;
        variable i : integer;
        function "??"(x : integer) return boolean;
    begin
        if b then end if;               -- OK
        if b xor '1' then end if;       -- OK
        while b and '1' loop end loop;  -- OK
        if i + 1 then end if;           -- OK
        if now + 1 ns then end if;      -- Error
        exit when b or '1';             -- OK
        next when b or '1';             -- OK
        wait until b xor '0';           -- OK
        assert b nor '1';               -- OK
        assert ?? 1;                    -- OK
    end process;

    /* This is a comment */

    /* Comments /* do not nest */

end architecture;
