package textio is
    type line is access string;
end package;

use     work.textio.all;
package PKG is
    procedure SCAN(
        variable  TEXT_LINE     : inout LINE;
                  TEXT_END      : in    integer;
                  START_POS     : in    integer;
                  FOUND         : out   boolean;
                  FOUND_LEN     : out   integer
    );
end package;
use     work.textio.all;
package body  PKG is
    procedure SCAN(
        variable  TEXT_LINE     : inout LINE;
                  TEXT_END      : in    integer;
                  START_POS     : in    integer;
                  FOUND         : out   boolean;
                  FOUND_LEN     : out   integer
    ) is
        variable  len           :       integer;
        variable  char          :       character;
    begin
        len      := 1;
        for pos in START_POS+1 to text_end loop
            char := text_line(pos);
            case char is
                when NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|
                     BS |HT |LF |VT |FF |CR |SO |SI |
                     DLE|DC1|DC2|DC3|DC4|NAK|SYN|ETB|
                     CAN|EM |SUB|ESC|FSP|GSP|RSP|USP|DEL =>
                    exit;
                when '['|']'|'{'|'}'|',' =>
                    for prev_pos in pos-1 downto START_POS loop
                        exit when (text_line(prev_pos) /= ' ');
                        len := len - 1;
                    end loop;
                    exit;
                when ':'=>
                    for prev_pos in pos-1 downto START_POS loop
                        exit when (text_line(prev_pos) /= ' ');
                        len := len - 1;
                    end loop;
                    exit;
                when '#' =>
                    for prev_pos in pos-1 downto START_POS loop
                        exit when (text_line(prev_pos) /= ' ');
                        len := len - 1;
                    end loop;
                    exit;
                when others => null;
            end case;
            len := len + 1;
        end loop;
        FOUND     := TRUE;
        FOUND_LEN := len;
    end procedure;
end package body;

use     work.textio.all;
library WORK;
use     WORK.PKG.all;
entity  issue262 is
end     issue262;
architecture MODEL of issue262 is
begin
    process
        variable  text_line  : LINE;
        variable  text_end   : integer;
        variable  found      : boolean;
        variable  found_len  : integer;
    begin
        --write(text_line, string'("{A:1}"));
        text_line := new string'("{A:1}");
        text_end  := 4;
        SCAN(text_line, text_end, 1, found, found_len);
        report boolean'image(found);
        report integer'image(found_len);
        assert found;
        assert found_len = 2;
        wait;
    end process;
end MODEL;
