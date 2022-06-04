package pack is

    type id_alloc_t is protected
        impure function next_id return integer;
    end protected;

    type rec_t is record
        id : integer;
    end record;

    impure function get_next_rec return rec_t;
end package;

package body pack is

    type id_alloc_t is protected body
        variable counter : integer := 0;

        impure function next_id return integer is
        begin
            counter := counter + 1;
            return counter;
        end function;
    end protected body;

    shared variable id_alloc : id_alloc_t;

    impure function get_next_rec return rec_t is
    begin
        return (id => id_alloc.next_id);
    end function;

end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic ( r1, r2 : rec_t );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        assert r1.id = 1;
        assert r2.id = 2;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity protected7 is
end entity;

architecture test of protected7 is
    constant cr1 : rec_t := get_next_rec;
    constant cr2 : rec_t := get_next_rec;
begin

    u: entity work.sub generic map ( cr1, cr2 );

end architecture;
