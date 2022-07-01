use std.textio.all;

package broken_module is
    type prot_t is protected
        procedure proc;
    end protected;
end package;

package body broken_module is
  type prot_t is protected body
    variable var : natural := 0;

    procedure file_proc is
      file fwrite : text;
      variable s : string(1 to 3);
    begin
        file_open(fwrite, "tmp", WRITE_MODE);
        file_close(fwrite);
        write(fwrite, s);               -- Write to closed file
    end procedure;

    procedure proc is
    begin
      file_proc;
      var := 0; -- Comment out this and it will not fail
    end procedure;
  end protected body;
end package body;

-------------------------------------------------------------------------------

entity debug3 is
end entity;

use work.broken_module.all;

architecture test of debug3 is
    shared variable p : prot_t;
begin

    process is
    begin
        p.proc;
        wait;
    end process;

end architecture;
