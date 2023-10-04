-------------------------------------------------------------------------------
--  Copyright (C) 2012-2021  Nick Gasson
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-------------------------------------------------------------------------------

library nvc;
use nvc.polyfill.all;

package body textio is

    function get_slice (str : in string; ifrom, ito : positive) return string is
        alias astr : string(1 to str'length) is str;
    begin
        return astr(ifrom to ito);
    end function;

    function get_char (str : in string; nth : in positive) return character is
        alias astr : string(1 to str'length) is str;
    begin
        return astr(nth);
    end function;

    impure function read_severity return severity_level is
    begin
        `if VHDL_VERSION < "2019" then
            return error;
        `else
            return std.env.GetVhdlReadSeverity;
        `end if
    end function;

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
            tmp := new string(1 to l'length - nchars);
            tmp.all := get_slice(l.all, 1 + nchars, l'length);
        end if;
        deallocate(l);
        l := tmp;
    end procedure;

    function is_whitespace (x : character) return boolean is
    begin
        return x = ' ' or x = CR or x = LF or x = HT
            or x = character'val(160);  -- NBSP
    end function;

    procedure skip_whitespace (l : inout line) is
        variable skip : natural := 0;
    begin
        if l /= null then
            while skip < l'length and is_whitespace(get_char(l.all, skip + 1)) loop
                skip := skip + 1;
            end loop;
            consume(l, skip);
        end if;
    end procedure;

    function tolower (x : character) return character is
        constant offset : natural := character'pos('a') - character'pos('A');
    begin
        if x >= 'A' and x <= 'Z' then
            return character'val(character'pos(x) + offset);
        else
            return x;
        end if;
    end function;

    function strcasecmp (x, y : string) return boolean is
    begin
        if x'length /= y'length then
            return false;
        else
            for i in 1 to x'length loop
                if tolower(x(i)) /= tolower(y(i)) then
                    return false;
                end if;
            end loop;
        end if;
        return true;
    end function;

    procedure read (l     : inout line;
                    value : out bit;
                    good  : out boolean ) is
    begin
        good := false;
        skip_whitespace(l);
        if l /= null and l.all'length > 0 then
            case get_char(l.all, 1) is
                when '0' =>
                    value := '0';
                    good := true;
                    consume(l, 1);
                when '1' =>
                    value := '1';
                    good := true;
                    consume(l, 1);
                when others =>
                    null;
            end case;
        end if;
    end procedure;

    procedure read (l     : inout line;
                    value : out bit )
    is
        variable good : boolean;
    begin
        read(l, value, good);
        assert good report "bit read failed" severity read_severity;
    end procedure;

    procedure read (l     : inout line;
                    value : out bit_vector;
                    good  : out boolean ) is
        variable consumed : natural := 0;
        variable char     : character;
    begin
        good := true;

        outer: for i in value'range loop
            loop
                if l.all'length <= consumed then
                    good := false;
                    exit outer;
                end if;
                char := get_char(l.all, consumed + 1);
                exit when not is_whitespace(char);
                consumed := consumed + 1;
            end loop;

            if char = '0' then
                value(i) := '0';
            elsif char = '1' then
                value(i) := '1';
            else
                good := false;
                exit;
            end if;
            consumed := consumed + 1;
        end loop;

        consume(l, consumed);
    end procedure;

    procedure read (l     : inout line;
                    value : out bit_vector )
    is
        variable good : boolean;
    begin
        read(l, value, good);
        assert good report "bit_vector read failed" severity read_severity;
    end procedure;

    procedure read (l     : inout line;
                    value : out boolean;
                    good  : out boolean ) is
    begin
        good := false;
        skip_whitespace(l);
        if l.all'length >= 4 and strcasecmp(get_slice(l.all, 1, 4), "true") then
            consume(l, 4);
            good := true;
            value := true;
        elsif l.all'length >= 5 and strcasecmp(get_slice(l.all, 1, 5), "false") then
            consume(l, 5);
            good := true;
            value := false;
        end if;
    end procedure;

    procedure read (l     : inout line;
                    value : out boolean )
    is
        variable good : boolean;
    begin
        read(l, value, good);
        assert good report "boolean read failed" severity read_severity;
    end procedure;

    procedure read (l     : inout line;
                    value : out character;
                    good  : out boolean ) is
    begin
        if l /= null and l'length > 0 then
            value := get_char(l.all, 1);
            consume(l, 1);
            good := true;
        else
            good := false;
        end if;
    end procedure;

    procedure read (l     : inout line;
                    value : out character )
    is
        variable good : boolean;
    begin
        read(l, value, good);
        assert good report "character read failed" severity read_severity;
    end procedure;

    procedure read (l     : inout line;
                    value : out integer;
                    good  : out boolean ) is
        variable pos         : integer := 1;
        variable digit       : integer;
        variable result      : integer := 0;
        variable is_negative : boolean := false;
        variable peek        : character;
    begin
        skip_whitespace(l);

        if pos <= l.all'length then
            case get_char(l.all, pos) is
                when '-' =>
                    pos := pos + 1;
                    is_negative := true;
                when '+' =>
                    pos := pos + 1;
                when others =>
            end case;
        end if;

        while pos <= l.all'length loop
            peek := get_char(l.all, pos);
            exit when peek < '0' or peek > '9';
            digit := character'pos(peek) - character'pos('0');
            if is_negative then
              digit := -digit;
            end if;
            result := (result * 10) + digit;
            pos := pos + 1;
        end loop;

        if is_negative and pos = 2 then
          -- Single dash without trailing digit is not good
          pos := 1;
        end if;

        good := pos > 1;
        value := result;
        consume(l, pos - 1);
    end procedure;

    procedure read (l     : inout line;
                    value : out integer )
    is
        variable good : boolean;
    begin
        read(l, value, good);
        assert good report "integer read failed" severity read_severity;
    end procedure;

    procedure read (l     : inout line;
                    value : out real;
                    good  : out boolean ) is
        variable whole    : integer;
        variable exponent : integer;
        variable result   : real;
        variable digit    : integer;
        variable shift    : integer := 1;
        variable pos      : integer := 2;
        variable sign     : character;
        variable rgood    : boolean;
        variable peek     : character;
    begin
        skip_whitespace(l);
        peek := get_char(l.all, 1);
        if l.all'length > 0 and (peek = '-' or peek = '+') then
            sign := get_char(l.all, 1);
            consume(l, 1);
        end if;
        read(l, whole, rgood);
        if not rgood then
            good := false;
            return;
        end if;
        result := real(whole);
        good := true;
        if l.all'length > 0 and get_char(l.all, 1) = '.' then
            while pos <= l.all'length loop
                peek := get_char(l.all, pos);
                exit when peek < '0' or peek > '9';
                digit := character'pos(peek) - character'pos('0');
                result := result + (real(digit) * (10.0 ** (-shift)));
                shift := shift + 1;
                pos := pos + 1;
            end loop;
            good := pos > 2;
            consume(l, pos - 1);
        end if;
        if l.all'length > 0 then
            peek := get_char(l.all, 1);
            if (peek = 'e' or peek = 'E') then
                consume(l, 1);
                read(l, exponent, rgood);
                if not rgood then
                    good := false;
                    return;
                end if;
                result := result * (10.0 ** exponent);
            end if;
        end if;
        if sign = '-' then
            value := -result;
        else
            value := result;
        end if;
    end procedure;

    procedure read (l     : inout line;
                    value : out real )
    is
        variable good : boolean;
    begin
        read(l, value, good);
        assert good report "real read failed" severity read_severity;
    end procedure;

    procedure read (l     : inout line;
                    value : out string;
                    good  : out boolean ) is
    begin
        if l /= null and value'length <= l'length then
            value := get_slice(l.all, 1, value'length);
            consume(l, value'length);
            good := true;
        else
            good := false;
        end if;
    end procedure;

    procedure read (l     : inout line;
                    value : out string )
    is
        variable good : boolean;
    begin
        read(l, value, good);
        assert good report "string read failed" severity read_severity;
    end procedure;

    procedure read (l     : inout line;
                    value : out time;
                    good  : out boolean ) is
        type unit_spec_t is record
            name   : string(1 to 3);
            length : positive;
            unit   : time;
        end record;

        type unit_map_t is array (natural range <>) of unit_spec_t;

        constant unit_map : unit_map_t := (
            ( "fs ", 2, fs ),
            ( "ps ", 2, ps ),
            ( "ns ", 2, ns ),
            ( "us ", 2, us ),
            ( "ms ", 2, ms ),
            ( "sec", 3, sec ),
            ( "min", 3, min ),
            ( "hr ", 2, hr ) );

        variable scale, len : integer;
        variable scale_good : boolean;
    begin
        good := false;
        skip_whitespace(l);
        read(l, scale, scale_good);
        if not scale_good then
            return;
        end if;
        skip_whitespace(l);
        for i in unit_map'range loop
            len := unit_map(i).length;
            if l'length >= len
                and strcasecmp(get_slice(l.all, 1, len), unit_map(i).name(1 to len))
            then
                value := scale * unit_map(i).unit;
                consume(l, len);
                good := true;
            end if;
        end loop;
    end procedure;

    procedure read (l     : inout line;
                    value : out time )
    is
        variable good : boolean;
    begin
        read(l, value, good);
        assert good report "time read failed" severity read_severity;
    end procedure;

    procedure sread (l      : inout line;
                     value  : out   string;
                     strlen : out   natural) is
        variable pos  : integer := 1;
        variable peek : character;
        alias avalue  : string(1 to value'length) is value;
    begin
        skip_whitespace(l);
        while pos <= l'right and pos <= avalue'right loop
            peek := get_char(l.all, pos);
            exit when is_whitespace(peek);
            avalue(pos) := peek;
            pos := pos + 1;
        end loop;
        consume(l, pos - 1);
        strlen := pos - 1;
    end procedure;

    procedure oread (l     : inout line;
                     value : out   bit_vector;
                     good  : out   boolean) is
        variable digits     : string(1 to (value'length + 2) / 3);
        variable ipos       : integer := 1;
        variable opos       : integer := 1;
        variable remainder  : integer;
        variable char       : character;
        variable bits       : bit_vector(1 to 3);
        variable underscore : boolean := false;
        alias avalue        : bit_vector(1 to value'length) is value;
    begin
        good := false;
        avalue := (others => '0');
        skip_whitespace(l);
        while ipos <= l'right and opos <= digits'right loop
            char := get_char(l.all, ipos);
            ipos := ipos + 1;
            if char = '_' and underscore then
                underscore := false;
                next;
            elsif char >= '0' and char <= '7' then
                digits(opos) := char;
                underscore := true;
                opos := opos + 1;
            else
                exit;
            end if;
        end loop;
        consume(l, ipos - 1);
        if opos /= digits'right + 1 then
            return;
        end if;
        opos := 1;
        remainder := avalue'length rem 3;
        for i in digits'range loop
            case digits(i) is
                when '0' => bits := "000";
                when '1' => bits := "001";
                when '2' => bits := "010";
                when '3' => bits := "011";
                when '4' => bits := "100";
                when '5' => bits := "101";
                when '6' => bits := "110";
                when '7' => bits := "111";
                when others => assert false;
            end case;
            if i = digits'left and remainder /= 0 then
                -- Partial copy of first digit
                avalue(1 to remainder) := bits(4 - remainder to 3);
                opos := opos + remainder;
                for j in 1 to 3 - remainder loop
                    if bits(j) /= '0' then
                        return;
                    end if;
                end loop;
            else
                avalue(opos to opos + 2) := bits;
                opos := opos + 3;
            end if;
        end loop;
        good := true;
    end procedure;

    procedure oread (l     : inout line;
                     value : out   bit_vector) is
        variable good : boolean;
    begin
        oread(l, value, good);
        assert good report "oread failed" severity read_severity;
    end procedure;

    procedure hread (l     : inout line;
                     value : out   bit_vector;
                     good  : out   boolean) is
        variable digits     : string(1 to (value'length + 3) / 4);
        variable ipos       : integer := 1;
        variable opos       : integer := 1;
        variable remainder  : integer;
        variable char       : character;
        variable bits       : bit_vector(1 to 4);
        variable underscore : boolean := false;
        alias avalue        : bit_vector(1 to value'length) is value;
    begin
        good := false;
        avalue := (others => '0');
        skip_whitespace(l);
        while ipos <= l'right and opos <= digits'right loop
            char := get_char(l.all, ipos);
            ipos := ipos + 1;
            if char = '_' and underscore then
                underscore := false;
                next;
            elsif (char >= '0' and char <= '9')
                or (char >= 'a' and char <= 'f')
                or (char >= 'A' and char <= 'F')
            then
                digits(opos) := char;
                underscore := true;
                opos := opos + 1;
            else
                exit;
            end if;
        end loop;
        consume(l, ipos - 1);
        if opos /= digits'right + 1 then
            return;
        end if;
        opos := 1;
        remainder := avalue'length rem 4;
        for i in digits'range loop
            case digits(i) is
                when '0' => bits := "0000";
                when '1' => bits := "0001";
                when '2' => bits := "0010";
                when '3' => bits := "0011";
                when '4' => bits := "0100";
                when '5' => bits := "0101";
                when '6' => bits := "0110";
                when '7' => bits := "0111";
                when '8' => bits := "1000";
                when '9' => bits := "1001";
                when 'a' | 'A' => bits := "1010";
                when 'b' | 'B' => bits := "1011";
                when 'c' | 'C' => bits := "1100";
                when 'd' | 'D' => bits := "1101";
                when 'e' | 'E' => bits := "1110";
                when 'f' | 'F' => bits := "1111";
                when others => assert false;
            end case;
            if i = digits'left and remainder /= 0 then
                -- Partial copy of first digit
                avalue(1 to remainder) := bits(5 - remainder to 4);
                opos := opos + remainder;
                for j in 1 to 4 - remainder loop
                    if bits(j) /= '0' then
                        return;
                    end if;
                end loop;
            else
                avalue(opos to opos + 3) := bits;
                opos := opos + 4;
            end if;
        end loop;
        good := true;
    end procedure;

    procedure hread (l     : inout line;
                     value : out   bit_vector) is
        variable good : boolean;
    begin
        hread(l, value, good);
        assert good report "hread failed" severity read_severity;
    end procedure;

    -- Allocator will round up to 128
    constant LINE_BUFFER_SIZE : positive := 127;

    procedure readline (file f: text; l: inout line) is
        variable tmp  : line;
        variable ch   : string(1 to 1);
        variable used : natural;
        variable got  : integer;
    begin
        if l /= null then
            deallocate(l);
        end if;

        tmp := new string(1 to LINE_BUFFER_SIZE);
        loop
            exit when endfile(f);

            read(f, ch, got);
            exit when got /= 1;

            next when ch(1) = CR;

            if ch(1) = LF then
                exit;
            else
                if used = tmp'length then
                    grow(tmp, LINE_BUFFER_SIZE, used);
                end if;
                used := used + 1;
                tmp(used) := ch(1);
            end if;
        end loop;

        shrink(tmp, used);
        l := tmp;
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

    procedure tee (file f : text; l : inout line) is
    begin
        if l /= null then
            write(f, l.all);
            write(output, l.all);
            deallocate(l);
        end if;
        write(f, (1 => LF));   -- Prepend CR on Windows?
        write(output, (1 => LF));
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
        width := maximum(value'length, field);
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

    function bit_to_char (b : bit) return character is
        type table_t is array (bit) of character;
        constant table : table_t := ( '0' => '0',
                                      '1' => '1' );
    begin
        return table(b);
    end function;

    procedure write (l         : inout line;
                     value     : in bit;
                     justified : in side := right;
                     field     : in width := 0 ) is
    begin
        write(l, bit_to_char(value), justified, field);
    end procedure;

    procedure write (l         : inout line;
                     value     : in bit_vector;
                     justified : in side := right;
                     field     : in width := 0 )
    is
        variable s : string(1 to value'length);
        alias v : bit_vector(1 to value'length) is value;
    begin
        for i in s'range loop
            s(i) := bit_to_char(v(i));
        end loop;
        write(l, s, justified, field);
    end procedure;

    procedure write (l         : inout line;
                     value     : in boolean;
                     justified : in side := right;
                     field     : in width := 0 ) is
    begin
        write(l, boolean'image(value), justified, field);
    end procedure;

    function unit_string (unit : time) return string is
    begin
        -- Standard requires unit in lower case
        if unit = fs then
            return " fs";
        elsif unit = ps then
            return " ps";
        elsif unit = ns then
            return " ns";
        elsif unit = us then
            return " us";
        elsif unit = ms then
            return " ms";
        elsif unit = sec then
            return " sec";
        elsif unit = min then
            return " min";
        elsif unit = hr then
            return " hr";
        else
            report "invalid unit " & time'image(unit);
        end if;
    end function;

    procedure write (l         : inout line;
                     value     : in time;
                     justified : in side := right;
                     field     : in width := 0;
                     unit      : in time := ns )
    is
        variable value_time: time := abs(value);
        variable digit_time: time := unit;
        variable str       : string (1 to 22);
        variable pos       : natural := str'left;
        variable digit     : integer;
    begin
        if value < 0 ns then
            str(pos) := '-';
            pos := pos + 1;
        end if;

        while value_time / 10 >= digit_time loop
            digit_time := digit_time * 10;
        end loop;

        while (pos <= str'right) loop
            digit      := value_time / digit_time;
            value_time := value_time - digit * digit_time;
            str(pos)   := character'val(digit + character'pos ('0'));
            pos        := pos + 1;
            exit when value_time = 0 fs and digit_time <= unit;
            if digit_time = unit and pos <= str'right then
                str(pos) := '.';
                pos := pos + 1;
            end if;
            exit when (digit_time / 10) * 10 /= digit_time;
            digit_time := digit_time / 10;
        end loop;
        write(l, str(1 to pos-1) & unit_string(unit), justified, field);
    end procedure;

    procedure write (l         : inout line;
                     value     : in real;
                     justified : in side := right;
                     field     : in width := 0;
                     digits    : in natural := 0 ) is
    begin
        if digits = 0 then
            write(l, to_string(value, "%e"), justified, field);
        else
            write(l, to_string(value, "%." & integer'image(digits) & "f"),
                  justified, field);
        end if;
    end procedure;

    procedure write (l         : inout line;
                     value     : in real;
                     format    : in string ) is
    begin
        write(l, to_string(value, format));
    end procedure;

    procedure write (l         : inout line;
                     value     : in integer;
                     justified : in side := right;
                     field     : in width := 0 ) is
    begin
        write(l, integer'image(value), justified, field);
    end procedure;

    procedure owrite (l         : inout line;
                      value     : in    bit_vector;
                      justified : in    side  := right;
                      field     : in    width := 0) is
    begin
        write(l, to_ostring(value), justified, field);
    end procedure;

    procedure hwrite (l         : inout line;
                      value     : in    bit_vector;
                      justified : in    side  := right;
                      field     : in    width := 0) is
    begin
        write(l, to_hstring(value), justified, field);
    end procedure;

    function justify (value     : string;
                      justified : side  := right;
                      field     : width := 0) return string
    is
        variable width  : natural := maximum(value'length, field);
        variable result : string(1 to width) := (others => ' ');
    begin
        if justified = left then
            result(1 to value'length) := value;
        else
            result(1 + width - value'length to width) := value;
        end if;
        return result;
    end function;

end package body;
