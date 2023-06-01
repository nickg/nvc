entity vhdl2 is
end entity;

architecture test of vhdl2 is
    signal s1 : bit_vector(1 to 6) := (1 => '1', others => '0');
    type t1 is array (natural range <>) of bit;
    subtype t2 is t1(1 to 99);

    function f1 (x, y : integer) return bit;

    procedure p1 is
        variable v1 : integer := 5;
    begin
        v1 := v1 + 1;
    end procedure;

    component c1 is
        generic ( g1 : integer := 2; type t );
        port ( x, y : out bit );
    end component;

    type t3 is record
        x, y : integer;
    end record;

begin
end architecture;
