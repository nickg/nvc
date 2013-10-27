package foo_pkg is
    type my_int is range 0 to 100;
    subtype my_int_sub is my_int range 10 to 20;
end package;

-------------------------------------------------------------------------------

use work.foo_pkg.all;

entity foo is
    port (
        o : out my_int;
        i : in my_int );
end entity;

-------------------------------------------------------------------------------

architecture bar of foo is
begin

    process is
        variable x : my_int;
    begin
        x := i;                         -- OK
    end process;

    process is
        variable x : my_int;
    begin
        -- Cannot read output
        x := o;
    end process;

    process is
    begin
        o <= 24;                        -- OK
    end process;

    process is
    begin
        -- Cannot assign input
        i <= 23;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

use work.foo_pkg.all;

architecture test of top is

    component foo is
        port (
            o : out my_int;
            i : in my_int );
    end component;

    type int_vec is array (integer range <>) of integer;

    component bar is
        port (
            i : in int_vec(1 to 10);
            o : out int_vec(1 to 2) );
    end component;

    signal x, y : my_int;
begin

    foo1: entity work.foo               -- OK
        port map (
            o => x,
            i => y );

    foo2: entity work.foo               -- OK
        port map ( x, y );

    foo3: entity work.foo
        ;                               -- Missing i association

    foo4: entity work.foo               -- Two associations for i
        port map ( i => x, i => y,
                   o => x );

    foo5: entity work.foo               -- Too many ports
        port map ( x, y, x, y );

    foo6: entity work.foo               -- No port cake
        port map ( cake => 4 );

    bad1: entity work.bad;              -- No such entity

    open1: entity work.foo              -- OK
        port map (
            i => x,
            o => open );

    open2: entity work.foo              -- Cannot use OPEN with input
        port map (
            i => open,
            o => open );

    foo7: foo                           -- OK
        port map (
            o => x,
            i => y );

    foo8: component foo                 -- OK
        port map (
            o => x,
            i => y );

    bad2: component x                   -- Not component
        port map (
            a => 1,
            b => 2 );

    b1: block is
        signal x : int_vec(1 to 10);
        signal y : int_vec(1 to 2);
        signal k : integer;
    begin

        bar1: bar                       -- OK
            port map (
                o(1 to 10) => x(1 to 10),
                i(1 to 2)  => y(1 to 2) );

        bar2: bar                       -- OK
            port map (
                o(1 to 4)  => x(1 to 4),
                o(5 to 10) => x(5 to 10),
                i(1 to 2)  => y(1 to 2) );

        bar3: bar
            port map (
                o(1)       => x(1),
                o(2)       => x(2),
                o(3 to 10) => x(3 to 10),
                i          => y );

        bar4: bar
            port map (
                o(1)       => x(1),
                o(2)       => x(k),     -- Error
                o(3 to 10) => x(3 to 10),
                i          => y );

        bar5: bar
            port map (
                o(1)       => x(1),
                o(q)       => x(2),     -- Error
                o(3 to 10) => x(3 to 10),
                i          => y );

        bar6: bar
            port map (
                o(1)      => x(1),
                o(2)      => x(2),
                o(3 to u) => x(3 to 10),  -- Error
                i         => y );

        bar7: bar
            port map (
                o(k)       => x(1),     -- Error
                o(2)       => x(2),
                o(3 to 10) => x(3 to 10),
                i          => y );

        bar8: bar
            port map (
                o(1)      => x(1),
                o(2)      => x(2),
                o(3 to k) => x(3 to 10),  -- Error
                i         => y );

    end block;

    foo9: foo                           -- Error
        port map (
            o => x,
            i => hello(5) );

    foo10: foo
        port map (
            i => y );                   -- OK

end architecture;

-------------------------------------------------------------------------------

architecture other of top is

    type int_vec is array (integer range <>) of integer;

    component comp1 is
        port (
            a : in integer := 5;
            o : out int_vec );
    end component;

    signal s : int_vec(1 to 3);

begin

    c1: component comp1                 -- OK
        port map (
            a => open,
            o => s );

    c2: component comp1
        port map (
            a => 5,
            o => open );                -- Error

    c3: component comp1
        port map (
            a => 1.0,                   -- Error
            o => s );

end architecture;
