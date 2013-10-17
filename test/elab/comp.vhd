entity e1 is
    generic ( g : integer );
    port (
        x : in integer;
        y : out integer );
end entity;

architecture test of e1 is
begin
end architecture;

-------------------------------------------------------------------------------

entity e2 is
    generic ( g : integer );
    port (
        x : in integer );
end entity;

architecture test of e2 is
begin
end architecture;

-------------------------------------------------------------------------------

entity e3 is
    generic ( g : integer );
    port (
        x : in integer;
        y : out integer );
end entity;

architecture test of e3 is
begin
end architecture;

-------------------------------------------------------------------------------

entity foo is
end entity;

architecture test of foo is
    component e1 is
        generic (
            g : integer);
        port (
            x : in  integer;
            y : out integer);
    end component;

    component e2 is
        generic (
            g : integer);
        port (
            y : out integer);
    end component;

    component e3 is
        generic (
            g : integer);
        port (
            x : in  bit;
            y : out integer);
    end component;

    signal x : integer;
    signal y : integer;
begin

    e1_1: e1                            -- OK
        generic map (
            g => 5 )
        port map (
            x => x,
            y => y);

    e2_1: e2                            -- Error
        generic map (
            g => 5 )
        port map (
            y => y);

    e3_1: e3                            -- Error
        generic map (
            g => 5 )
        port map (
            x => '1',
            y => y);


end architecture;
