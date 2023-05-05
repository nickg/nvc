-- compile to library "test"

package pkg is
        procedure reset;
end package pkg;

-------------------------------------------------------------------------------

library test;
use test.pkg;

entity ent is
end entity ent;

architecture test of ent is -- Change the architecture to anything but "test" to make it work
begin
        p : process is
        begin
                pkg.reset;
                wait;
        end process p;
end architecture test;
