package inner is
    constant a : integer := 42 ;
end package ;

package outer is
    alias inner is work.inner ;
    constant b : integer := inner.a;       -- OK
end package ;

use work.outer.all ;

entity test is
end entity ;

architecture arch of test is
    constant c : integer := inner.a;    -- OK
    use inner.all;
    constant d : integer := a;          -- OK
begin
end architecture ;
