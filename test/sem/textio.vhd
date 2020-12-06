package textio is

    type line is access string;

    type text is file of string;

    type side is (RIGHT, LEFT);

    subtype width is natural;

    file input : text open READ_MODE is "STD_INPUT";

    file output : text open WRITE_MODE is "STD_OUTPUT";

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
            tmp := new string(1 to l'length + extra);  -- OK
            tmp(1 to l'length) := l.all;
            deallocate(l);
            l := tmp;
        end if;
    end procedure;

    procedure shrink (l : inout line; size : in natural) is
        variable tmp : line;
    begin
        assert l /= null;
        if size < l'length then
            tmp := new string(1 to size);
            tmp.all := l.all(1 to size);
            deallocate(l);
            l := tmp;
        end if;
    end procedure;

    procedure consume (l : inout line; nchars : in natural) is
        variable tmp : line;
    begin
        if nchars = 0 then
            return;
        end if;
        assert l /= null;
        if nchars = l'length then
            tmp := new string'("");
        else
            assert nchars <= l'length;
            tmp := new string(1 to l'length - nchars);  -- OK
            tmp.all := l.all(1 + nchars to l'length);
        end if;
        deallocate(l);
        l := tmp;
    end procedure;

end package body;
