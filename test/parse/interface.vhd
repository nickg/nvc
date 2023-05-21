package pack045c is

    type rec_t is record
        a   :   integer ;
        b   :   string ;
        c   :   bit_vector(7 downto 0) ;
        d   :   bit_vector(0 to -1) ;
    end record ;

    view master of rec_t is
        a   :   in ;
        b   :   out ;
        c   :   out ;
        d   :   inout ;
    end view ;

    alias slave is master'converse ;

end package ;
