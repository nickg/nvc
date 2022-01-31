entity default2 is
end entity;

architecture test of default2 is

    procedure proc1 (x : integer) is
        type real_vec is array (integer range <>) of real;
        variable v : real_vec(1 to x);
    begin
        assert v(1) = real'left;
        assert v(x) = real'left;
    end procedure;

    procedure proc2 (x : integer) is
        type rec is record
            x : real;
            y : bit_vector(1 to x);
        end record;
        type rec_vec is array (natural range <>) of rec;
        variable v : rec_vec(1 to x);
    begin
        assert v(1).x = real'left;
        assert v(1).y = (1 to x => '0');
        assert v(x).x = real'left;
        assert v(x).y = (1 to x => '0');
    end procedure;

    procedure proc3 (x : integer) is
        type int_ptr is access integer;
        type rec is record
            x : int_ptr;
        end record;
        type rec_vec is array (natural range <>) of rec;
        variable v : rec_vec(1 to x);
    begin
        assert v(1).x = null;
        assert v(x).x = null;
    end procedure;

begin

    main: process is
    begin
        proc1(5);
        proc2(6);
        proc3(2);
        wait;
    end process;

end architecture;
