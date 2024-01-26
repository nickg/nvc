entity vhdl2 is
end entity;

architecture test of vhdl2 is
    signal s1 : bit_vector(1 to 6) := (1 => '1', others => '0');
    type t1 is array (natural range <>) of bit;
    subtype t2 is t1(1 to 99);

    function f1 (x, y : integer) return bit;

    procedure proc1 is
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

    function fact (n : natural) return natural is
    begin
        if n > 1 then
            return n * fact(n - 1);
        else
            return 1;
        end if;
    end function;

begin

    b1: block is
        port ( p : integer );
        port map ( p => inertial 1 );
    begin
    end block;

    u1: component c1
        generic map ( t => integer )
        port map ( open, y => s1(0) );

    p1: process (s1) is
    begin
        s1 <= "101010" after 1 ns, (others => '1') after 2 ns;
    end process;

    g1: for i in 5 downto 2 generate
        attribute a : integer;
        signal r : real := 1.2345;
        attribute a of r : signal is 5;
    begin
        assert r > 0.0;
    end generate;

end architecture;
