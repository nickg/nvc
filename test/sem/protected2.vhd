package p is
    type t_protected is protected
    end protected;

    constant  c : t_protected;          -- Error
end package;

package body p is
    type t_protected is protected body
    end protected body;
end package body;

use work.p.all;

entity e1 is
end entity;

architecture a1 of e1 is

    type bad_file_type is file of t_protected;   -- Error

    type t_protected_array  is array (0 to 1) of t_protected;   -- Error

    type t_protected_record is record
        a   : integer;
        b   : t_protected;              -- Error
        c   : real;
    end record;

    signal    s : t_protected;          -- Error
    attribute t : t_protected;          -- Error

    component bad_gen is
    generic (
    g1 : t_protected                    -- Error
    );
    end component;

    component bad_port is
    port (
    p1 : t_protected                    -- Error
    );
    end component;

begin

end architecture;
