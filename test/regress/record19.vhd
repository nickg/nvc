entity record19 is
end entity;

architecture test of record19 is

    type data_t is record
        x, y : bit;
    end record;

    type data_vector is array (natural range <>) of data_t;

    procedure mux (signal data_i : in data_vector(1 to 7);
                   selected : in integer;
                   signal data_o : out data_t )
    is
        variable tmp : data_t;
    begin
        tmp := data_i(selected);
        data_o <= tmp;
    end procedure;


    signal di : data_vector(1 to 7);
    signal do : data_t;

begin

    main: process is
    begin
        di <= ( 2 => ('1', '1'), others => ('0', '0') );
        wait for 1 ns;
        mux(di, 1, do);
        wait for 1 ns;
        assert do = ( '0', '0' );
        mux(di, 2, do);
        wait for 1 ns;
        assert do = ( '1', '1' );
        wait;
    end process;

end architecture;
