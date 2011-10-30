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
    signal x, y : my_int;
begin

    foo1: entity work.foo               -- OK
        port map (
            o => x,
            i => y );

    foo2: entity work.foo               -- OK
        port map ( x, y );

    foo3: entity work.foo
        port map ( i => x );            -- Missing o association

    foo4: entity work.foo               -- Two associations for i
        port map ( i => x, i => y,
                   o => x );

    foo5: entity work.foo               -- Too many ports
        port map ( x, y, x, y );

    foo6: entity work.foo               -- No port cake
        port map ( cake => 4 );

    bad1: entity work.bad;              -- No such entity   
    
end architecture;
