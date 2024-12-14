package pkg is
    type vec is array(0 to 3) of integer;
end pkg;

use work.pkg.all;

entity ent is
    port (
        p0 : boolean := vec'(0=>0, 1=>0) = vec'(3=>0);
        p1 : boolean := vec'(0=>0, 1=>1, 1 to 2=>1) = vec'(others=>0);
        p2 : boolean := vec'(0=>0, 1=>1, 2 to 3=>1) = vec'(others=>0)
    );
end ent;

architecture arch of ent is
begin
end architecture arch;
