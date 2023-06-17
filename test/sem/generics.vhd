entity bot is
    generic ( N : integer );
    port ( o : out integer );
end entity;

architecture a of bot is
begin

    process is
    begin
        o <= N;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
    signal x : integer;
begin

    bot0: entity work.bot               -- OK
        generic map ( N => 5 )
        port map ( o => x );

    bot1: entity work.bot               -- OK
        generic map ( 5 )
        port map ( o => x );

    bot3: entity work.bot
        port map ( o => x );            -- Missing N

    bot4: entity work.bot
        generic map ( 1, 2 )            -- Too many generics
        port map ( o => x );

end architecture;

-------------------------------------------------------------------------------

entity bad is
    generic (
        X : integer;
        Y : integer := X + 1 );         -- X not visible
    port (
        p : in integer := X );
end entity;

-------------------------------------------------------------------------------

entity class is
    generic (
        constant X : integer;           -- OK
        signal Y : integer );           -- Error
end entity;

-------------------------------------------------------------------------------

package p is

    component c is
        generic ( X : integer );        -- OK
        port ( p : in integer range 1 to X;  -- OK
               q : in integer range 1 to Y );  -- Error
    end component;

end package;

-------------------------------------------------------------------------------

entity static is
    generic ( X : integer );
end entity;

architecture a of static is
    constant k : integer := X + 1;
    signal s : bit_vector(1 to 3);
    signal s2 : integer;
    alias sx : bit is s(X);
    alias sx1 : bit is s(X + 1);
    alias sx2 : bit_vector is s(k to 3);

    function f(x : bit_vector) return integer;

    component c is
        generic (
            x : bit_vector(2 downto 0) );
    end component;

    component d is
        generic (
            t : time );
    end component;
begin

    i1: entity work.bot
        generic map (
            N => f("100") )
        port map (
            o => open );

    i2: component c
        generic map ( x => "00" & '1' );  -- OK

    i3: component c
        generic map ( x => "00" & sx );  -- Error

    i4: component d
        generic map ( t => 100 ns );    -- OK

    i5: component c
        generic map ( 6 => 1 );         -- Error

    i6: component c
        generic map ( "not"(x) => "101" );  -- Error

    i7: component c
        generic map ( i6 );             -- Error

    i8: component c
        generic map ( a );              -- Error

    i9: component c
        generic map ( std.standard );   -- Error

    i10: component c
        generic map ( x(s2 downto 0) => "00" );  -- Error

end architecture;
