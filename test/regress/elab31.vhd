package pack is

    type rec is record
        x : integer;
    end record;

    type pt is protected
        impure function next_id return integer;
    end protected;

    impure function get_rec return rec;

    function get_id (r : rec) return integer;

end package;

package body pack is

    type pt is protected body
        variable ctr : integer := 0;

        impure function next_id return integer is
        begin
            ctr := ctr + 1;
            return ctr;
        end function;
    end protected body;

    shared variable p : pt;

    impure function get_rec return rec is
    begin
        return (x => p.next_id);
    end function;

    function get_id (r : rec) return integer is
    begin
        return r.x;
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic ( r1, r2 : rec );
    port ( s1 : in bit_vector(1 to get_id(r1));
           s2 : in bit_vector(1 to get_id(r2)) );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        assert s1 = "0";
        assert s2 = "00";
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity elab31 is
end entity;

architecture test of elab31 is
    constant r1, r2 : rec := get_rec;
    signal s1 : bit_vector(1 to 1);
    signal s2 : bit_vector(1 to 2);
begin

    u: entity work.sub generic map ( r1, r2 ) port map ( s1, s2 );

end architecture;
