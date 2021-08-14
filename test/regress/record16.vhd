entity record16 is
end entity;

architecture test of record16 is
    type rec is record
        x : bit;
        y : integer;
    end record;

    signal r : rec := ('1', 0);

    procedure drive(signal s : out integer; value : in integer) is
    begin
        s <= value;
    end procedure;

    procedure read(signal s : in integer; value : out integer) is
    begin
        value := s;
    end procedure;

begin

    process is
        variable x : integer;
    begin
        drive(r.y, 123);
        wait for 1 ns;
        assert r.y = 123;
        read(r.y, x);
        --report integer'image(x);
        --assert x = 123;
        wait;
    end process;

end architecture;
