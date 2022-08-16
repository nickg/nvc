entity names2 is
end entity;

architecture test of names2 is
    type array1 is array (natural range <>) of integer;
    type array2 is array (natural range <>) of array1(1 to 10);
    type array3 is array (natural range <>) of array2(1 to 10);

    type rec is record
        x : array1(1 to 3);
        y : integer;
    end record;

    function get_array3(n : integer) return array3;
    function get_array2 return array2;

    procedure do_rec(x : in rec);

    type pt is protected
        function get_array1(n : integer) return array1;
    end protected;
begin

    p1: process is
        variable p : pt;
    begin
        assert get_array3(1)(2)(2)(3) = 1;  -- OK
        --assert get_array2(2)(3) = 1;    -- OK   (not yet)
        assert p.get_array1(1)(2) = 1;  -- OK
    end process;

    p2: process is
    begin
        do_rec(x.x => (1, 2, 3), x.y => 5);  -- OK
    end process;

end architecture;
