package lcs2016_45a is

    type rec_t is record
        a   :   integer ;
        b   :   string ;
        c   :   bit_vector(7 downto 0) ;
        d   :   bit_vector(0 to -1) ;
    end record ;

    view master1 of rec_t is
        a   :   in ;
        b   :   out ;
        c   :   out ;
        d   :   inout ;
    end view ;

    --alias slave is master'converse ;

    view master2 of rec_t is
        a, x : in;                      -- Error
    end view;

    view master3 of rec_t is
        a, d : in;                      -- Error
    end view;

    view bad1 of integer is             -- Error
        a, d : in;                      -- Error
    end view;

    type rec_vec_t is array (natural range <>) of rec_t;

    function resolved (r : rec_vec_t) return rec_t;

    subtype rrec_t is resolved rec_t;

    view master4 of rrec_t is           -- Error
        a, b, c, d : in;
    end view;

    view bad2 of rec_t is
        integer : in;                   -- Error
        a, b, c, d : in;
    end view;

    view master5 of rec_t is
        a, b, c, d : in;
        b : out;                        -- Error
    end view;

    constant k : master1;               -- Error

    alias a1 is integer'converse;       -- Error

    alias a2 is master1;                -- OK

    alias a3 is a2'converse;            -- OK

    component comp1 is
        port ( x : view master1 );      -- OK
    end component;

    component comp2 is
        port ( x : view rec_t;          -- Error
               y : view master1 of integer;  -- Error
               z : view master1 of rec_t;  -- Ok
            );
    end component;

    view master6 of rec_t is
        a   :   in ;
        b   :   out ;
        c   :   inout ;
        d   :   linkage ;               -- Error
    end view ;

    type outer_rec_t is record
        r : rec_t;
    end record ;

    view outer_master1 of outer_rec_t is
        r : view master1;               -- OK
    end view;

    view outer_master2 of outer_rec_t is
        r : view master1 of integer;    -- Error
    end view;

    view outer_master3 of outer_rec_t is
        r : view integer of rec_t;    -- Error
    end view;

end package ;

-------------------------------------------------------------------------------

package pack is

    type rec_t is record
        a, b : bit;
    end record;

    view master_rec_t of rec_t is          -- OK
        a : in;
        b : out;
    end view;

    alias slave_rec_t is master_rec_t'converse;  -- OK

end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( r : view slave_rec_t of rec_t );  -- OK
end entity;
