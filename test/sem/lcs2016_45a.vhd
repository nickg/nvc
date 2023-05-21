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


end package ;
