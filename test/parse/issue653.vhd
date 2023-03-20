package axi4s is

    type axis_t is record
        data    :   bit_vector ;
        dest    :   bit_vector ;
        id      :   bit_vector ;
        strb    :   bit_vector ;
        keep    :   bit_vector ;
        user    :   bit_vector ;
        last    :   bit ;
        valid   :   bit ;
        ready   :   bit ;
    end record ;

    type axis_array_t is array(natural range <>) of axis_t ;

    package make is
      generic (
        DATA_BYTES  :   positive    := 4 ;
        DEST_WIDTH  :   natural     := 0 ;
        ID_WIDTH    :   natural     := 0 ;
        USER_WIDTH  :   natural     := 0
      ) ;

        subtype DATA_RANGE is natural range DATA_BYTES*8-1 downto 0 ;
        subtype DEST_RANGE is natural range DEST_WIDTH-1 downto 0 ;
        subtype ID_RANGE   is natural range ID_WIDTH-1 downto 0 ;
        subtype KEEP_RANGE is natural range DATA_BYTES-1 downto 0 ;
        subtype USER_RANGE is natural range USER_WIDTH-1 downto 0 ;

        subtype axis_t is axi4s.axis_t(
            data(DATA_RANGE),
            dest(DEST_RANGE),
            id(ID_RANGE),
            keep(KEEP_RANGE),
            strb(KEEP_RANGE),
            user(USER_RANGE)
        ) ;

    end package ;

end package ;

package axis32 is new work.axi4s.make ;
