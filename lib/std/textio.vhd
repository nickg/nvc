--
-- TEXTIO package as defined by IEEE 1076-1993
--
package textio is

    type line is access string;

    type text is file of string;

    type side is (RIGHT, LEFT);

    subtype width is natural;

    file input : text open READ_MODE is "STD_INPUT";

    file output : text open WRITE_MODE is "STD_OUTPUT";

    procedure readline (file f: text; l: inout line);

    procedure read (l     : inout line;
                    value : out bit;
                    good  : out boolean );
    procedure read (l     : inout line;
                    value : out bit );

    procedure read (l     : inout line;
                    value : out bit_vector;
                    good  : out boolean );
    procedure read (l     : inout line;
                    value : out bit_vector );

    procedure read (l     : inout line;
                    value : out boolean;
                    good  : out boolean );
    procedure read (l     : inout line;
                    value : out boolean );

    procedure read (l     : inout line;
                    value : out character;
                    good  : out boolean );
    procedure read (l     : inout line;
                    value : out character );

    procedure read (l     : inout line;
                    value : out integer;
                    good  : out boolean );
    procedure read (l     : inout line;
                    value : out integer );

    procedure read (l     : inout line;
                    value : out real;
                    good  : out boolean );
    procedure read (l     : inout line;
                    value : out real );

    procedure read (l     : inout line;
                    value : out string;
                    good  : out boolean );
    procedure read (l     : inout line;
                    value : out string );

    procedure read (l     : inout line;
                    value : out time;
                    good  : out boolean );
    procedure read (l     : inout line;
                    value : out time );

    procedure writeline (file f : text; l : inout line);

    procedure write (l         : inout line;
                     value     : in bit;
                     justified : in side:= right;
                     field     : in width := 0 );

    procedure write (l         : inout line;
                     value     : in bit_vector;
                     justified : in side:= right;
                     field     : in width := 0 );

    procedure write (l         : inout line;
                     value     : in boolean;
                     justified : in side:= right;
                     field     : in width := 0 );

    procedure write (l         : inout line;
                     value     : in character;
                     justified : in side:= right;
                     field     : in width := 0 );

    procedure write (l         : inout line;
                     value     : in integer;
                     justified : in side:= right;
                     field     : in width := 0 );

    procedure write (l         : inout line;
                     value     : in real;
                     justified : in side:= right;
                     field     : in width := 0;
                     digits    : in natural:= 0 );

    procedure write (l         : inout line;
                     value     : in string;
                     justified : in side:= right;
                     field     : in width := 0 );

    procedure write (l         : inout line;
                     value     : in time;
                     justified : in side:= right;
                     field     : in width := 0;
                     unit      : in time:= ns );

end package;

package body textio is

    procedure grow (l        : inout line;
                    extra    : in natural;
                    old_size : out natural ) is
        variable tmp : line;
    begin
        if l = null then
            l := new string(1 to extra);
            old_size := 0;
        elsif extra > 0 then
            old_size := l'length;
            tmp := new string(1 to l'length + extra);
            tmp(1 to l'length) := l.all;
            deallocate(l);
            l := tmp;
        end if;
    end procedure;

    procedure shrink (l : inout line; size : in natural) is
        variable tmp : line;
    begin
        assert l /= null;
        assert size < l'length;
        tmp := new string(1 to size);
        tmp.all := l.all(1 to size);
        deallocate(l);
        l := tmp;
    end procedure;

    procedure readline (file f: text; l: inout line) is
        variable tmp  : line;
        variable ch   : string(1 to 1);
        variable used : natural;
        variable got  : integer;
    begin
        if l /= null then
            deallocate(l);
        end if;

        tmp := new string(1 to 128);
        loop
            exit when endfile(f);

            read(f, ch, got);
            exit when got /= 1;

            next when ch(1) = CR;

            if ch(1) = LF then
                null;
            else
                if used = tmp'length then
                    grow(tmp, 128, used);
                end if;
                used := used + 1;
                tmp(used) := ch(1);
            end if;
        end loop;

        if used = 0 then
            l := new string'("");
        else
            shrink(tmp, used);
            l := tmp;
        end if;
    end procedure;

    procedure writeline (file f : text; l : inout line) is
    begin
        if l /= null then
            write(f, l.all);
            deallocate(l);
        end if;
        write(f, (1 => LF));   -- Prepend CR on Windows?
        l := new string'("");
    end procedure;

    procedure write (l         : inout line;
                     value     : in string;
                     justified : in side := right;
                     field     : in width := 0 )
    is
        variable orig  : natural;
        variable width : natural;
    begin
        if value'length > field then
            width := value'length;
        else
            width := field;
        end if;
        grow(l, width, orig);
        if justified = left then
            l(orig + 1 to orig + value'length) := value;
            for i in orig + value'length + 1 to orig + width loop
                l(i) := ' ';
            end loop;
        else
            for i in orig + 1 to orig + width - value'length loop
                l(i) := ' ';
            end loop;
            l(orig + 1 + width - value'length to orig + width) := value;
        end if;
    end procedure;

    procedure write (l         : inout line;
                     value     : in character;
                     justified : in side := right;
                     field     : in width := 0 ) is
    begin
        write(l, string'(1 => value), justified, field);
    end procedure;

    procedure write (l         : inout line;
                     value     : in bit;
                     justified : in side := right;
                     field     : in width := 0 ) is
    begin
        write(l, bit'image(value), justified, field);
    end procedure;

end package body;
