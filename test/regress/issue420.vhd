package globals is

  signal COMMAND_FILE_ENDIAN  : bit;
  signal COMMAND_FILE_NAME    : string(1 to 1024);
  signal COMMAND_FILE_NAMELEN : integer;
  signal COMMAND_FILE_TARGET  : integer;
  signal COMMAND_FILE_START   : bit;
  signal COMMAND_FILE_ACK     : bit;

  procedure change (signal what : out bit);

end package globals;

package body globals is

    procedure change (signal what : out bit) is
    begin
        what <= '1';
    end procedure;

end package body;

-------------------------------------------------------------------------------

entity issue420 is
end entity;

use work.globals.all;

architecture test of issue420 is
begin

    check: process is
    begin
        assert COMMAND_FILE_ACK = '0';
        assert COMMAND_FILE_NAME = (1 to 1024 => NUL);
        change(COMMAND_FILE_ACK);
        wait for 1 ns;
        assert COMMAND_FILE_ACK = '1';
        wait;
    end process;

end architecture;
