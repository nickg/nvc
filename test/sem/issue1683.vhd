package issue1683_pkg is
    type rec_t is record
        flag : boolean;
    end record;
end package;

use work.issue1683_pkg.all;

entity issue1683 is
    generic (
        r : rec_t := (flag => true)
    );
end entity;

architecture test of issue1683 is
    alias flag : boolean is r.flag;
begin
    gen: if flag generate
    end generate;
end architecture;
