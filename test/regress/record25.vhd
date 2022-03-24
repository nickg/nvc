entity record25 is
end entity;

architecture test of record25 is
    type rec is record
        x, y: integer;
    end record;

    type rec_array is array (natural range <>) of rec;

    procedure check (signal x : in rec_array) is
    begin
        assert x = ( (1, 2), (3, 4) );
    end procedure;

    signal s1 : rec_array(1 to 2);
begin

    main: process is
    begin
        s1 <= ( (1, 2), (3, 4) );
        wait for 1 ns;
        check(s1);
        wait;
    end process;

end architecture;
