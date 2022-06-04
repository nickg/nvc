package protfold2_pack is

    type id_alloc_t is protected
        impure function next_id return integer;
    end protected;

    type rec_t is record
        id : integer;
    end record;

    impure function get_next_rec return rec_t;
end package;

package body protfold2_pack is

    type int_ptr_t is access integer;

    type ptr_array_t is array (natural range <>) of int_ptr_t;

    type id_alloc_t is protected body
        variable counter : integer := 0;
        variable ptrs : ptr_array_t(1 to 5);

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

use work.protfold2_pack.all;

entity protfold2_sub is
    generic ( r1, r2 : rec_t );
end entity;

architecture test of protfold2_sub is
begin

    g1: if r1.id = 1 generate
    begin
        p1: process is
        begin
            assert r1.id = 1;
            wait;
        end process;
    end generate;

    g2: if r2.id = 2 generate
    begin
        p1: process is
        begin
            assert r2.id = 2;
            wait;
        end process;
    end generate;

end architecture;

-------------------------------------------------------------------------------

use work.protfold2_pack.all;

entity protfold2 is
end entity;

architecture test of protfold2 is
    constant cr1 : rec_t := get_next_rec;
    constant cr2 : rec_t := get_next_rec;
begin

    u: entity work.protfold2_sub generic map ( cr1, cr2 );

end architecture;
