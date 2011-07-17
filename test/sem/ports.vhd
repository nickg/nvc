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
