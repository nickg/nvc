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

    bot3: entity work.bot               -- Missing N
        port map ( o => x );

    bot4: entity work.bot               -- Too many generics
        generic map ( 1, 2 )
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
    end component;

end package;

-------------------------------------------------------------------------------

entity static is
    generic ( X : integer );
end entity;

architecture a of static is
    constant k : integer := X + 1;
    signal s : bit_vector(1 to 3);
    alias sx : bit is s(X);
    alias sx1 : bit is s(X + 1);
    alias sx2 : bit_vector is s(k to 3);

    function f(x : bit_vector) return integer;
begin

    i: entity work.bot
        generic map (
            N => f("100") )
        port map (
            o => open );

end architecture;
