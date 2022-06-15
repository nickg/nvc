package test_pkg is
  shared variable shared_var : boolean  := false;
  procedure log(
    va1    : boolean    := shared_var
  );
end package;

package body test_pkg is
    procedure log( va1    : boolean    := shared_var  ) is
    begin
        report "va1 is " & boolean'image(va1);
    end procedure;
end package body;

-------------------------------------------------------------------------------

entity issue469 is
end entity;

use work.test_pkg.all;

architecture test of issue469 is
begin

    p1: process is
    begin
        log;
        shared_var := true;
        log;
        wait;
    end process;

end architecture;
