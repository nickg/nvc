-------------------------------------------------*- coding: iso-8859-1; -*-----
--  Copyright (C) 2023  Nick Gasson
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

use nvc.polyfill.all;

package body text_util is

    procedure tb_cat (tb : out text_buf_t; str : in string) is
        variable tmp    : str_ptr_t;
        constant nchars : natural := str'length;
    begin
        if tb.len + nchars + 1 >= tb.alloc then
            tmp := tb.buf;
            tb.alloc := maximum(64, maximum(tb.alloc + nchars, tb.alloc * 2));
            tb.buf := new string(1 to tb.alloc);
            if tmp /= null then
                tb.buf(1 to tb.len) := tmp(1 to tb.len);
                deallocate(tmp);
            end if;
        end if;

        tb.buf(tb.len + 1 to tb.len + nchars) := str;
        tb.len := tb.len + nchars;
    end procedure;

    function next_delimiter (s : string; pos : natural) return string is
        constant len  : integer := s'length;
        variable nest : natural := 0;
        alias ss      : string(1 to len) is s;
    begin
        for i in 1 + pos to len loop
            if nest = 0 and (ss(i) = ',' or ss(i) = ')') then
                return ss(1 + pos to i - 1);
            elsif ss(i) = ')' then
                nest := nest - 1;
            elsif ss(i) = '(' then
                nest := nest + 1;
            end if;
        end loop;
        return ss(1 + pos to len);
    end function;

    function count_delimiters (s : string) return natural is
        variable nest   : natural := 0;
        variable result : natural := 1;
    begin
        for i in s'range loop
            if nest = 1 and s(i) = ',' then
                result := result + 1;
            elsif s(i) = '(' then
                nest := nest + 1;
            elsif s(i) = ')' and nest > 0 then
                nest := nest - 1;
            end if;
        end loop;
        return result;
    end function;

    function find_open (s : string) return natural is
        constant len : integer := s'length;
        alias ss     : string(1 to len) is s;
    begin
        for i in 1 to len loop
            if ss(i) = '(' then
                return i;
            elsif ss(i) /= ' ' then
                exit;
            end if;
        end loop;
        report "failed to parse '" & s & "' (missing opening '(')"
            severity failure;
    end function;

    procedure trim_ws (s : in string; first, last : out natural) is
        constant len : integer := s'length;
        alias ss     : string(1 to len) is s;
    begin
        for i in 1 to len loop
            first := i - 1;
            if ss(i) /= ' ' then
                exit;
            end if;
        end loop;
        for i in len downto 1 loop
            last := i - 1;
            if ss(i) /= ' ' then
                exit;
            end if;
        end loop;
    end procedure;

    function find_quote (s : string) return natural is
        constant len : integer := s'length;
        alias ss     : string(1 to len) is s;
    begin
        for i in 1 to len loop
            if ss(i) = '"' then
                return i;
            elsif ss(i) /= ' ' then
                return 0;
            end if;
        end loop;
    end function;

    function find_unquote (s : string; pos : natural) return natural is
        constant len : integer := s'length;
        alias ss     : string(1 to len) is s;
    begin
        for i in 1 + pos to len loop
            if ss(i) = '"' and (i = len or ss(i + 1) /= '"') then
                return i - 1;
            end if;
        end loop;
        report "failed to parse '" & s & "' (missing closing '""')"
            severity failure;
    end function;

    procedure find_close (s : string; pos : natural) is
        constant len : integer := s'length;
        alias ss     : string(1 to len) is s;
    begin
        for i in pos to len loop
            if ss(i) = ')' then
                return;
            elsif ss(i) /= ' ' then
                exit;
            end if;
        end loop;
        report "failed to parse '" & s & "' (missing closing ')')"
            severity failure;
    end procedure;

    procedure report_bad_char (s : string; c : character) is
    begin
        report "invalid character " & character'image(c)
            & " in string """ & s & """" severity failure;
    end procedure;

    function toupper (x : in character) return character is
        constant xp : natural := character'pos(x);
    begin
        if xp >= 97 and xp <= 122 then
            return character'val(xp - 32);
        else
            return x;
        end if;
    end function;

    function isspace (x : in character) return boolean is
    begin
        return x = ' ' or x = HT or x = LF or x = VT or x = FF
            or x = CR or x = ' ';
    end function;

    function isdigit (x : in character) return boolean is
        constant xp : natural := character'pos(x);
    begin
        return xp >= 48 and xp <= 57;
    end function;

    function canon_value (s : string) return string is
        constant length  : natural := s'length;
        alias ss         : string(1 to length) is s;
        variable result  : string(1 to length);
        variable trimmed : positive := 1;
        variable char    : character;
        variable upcase  : boolean := true;
    begin
        for i in 1 to length loop
            char := ss(i);
            if isspace(char) then
                next;
            elsif char = ''' then
                upcase := not upcase;
            end if;
            if upcase then
                result(trimmed) := toupper(char);
            else
                result(trimmed) := char;
            end if;
            trimmed := trimmed + 1;
        end loop;
        return result(1 to trimmed - 1);
    end function;

    procedure string_to_int (s     : in string;
                             value : out t_int64;
                             used  : out natural)
    is
        constant length      : natural := s'length;
        alias ss             : string(1 to length) is s;
        variable pos         : positive := 1;
        variable is_negative : boolean;
        variable result      : t_int64 := 0;
        variable num_digits  : natural;
        constant ascii_zero  : natural := character'pos('0');
        variable ascii_value : natural;
        variable digit       : natural;
    begin
        while pos <= length and isspace(ss(pos)) loop
            pos := pos + 1;
        end loop;

        is_negative := pos <= length and ss(pos) = '-';
        if is_negative then
            pos := pos + 1;
        end if;

        while pos <= length and (isdigit(ss(pos)) or ss(pos) = '_') loop
            if ss(pos) /= '_' then
                ascii_value := character'pos(ss(pos));
                digit := ascii_value - ascii_zero;
                if result >= 0 then
                    result := (result * 10) + t_int64(digit);
                else
                    result := (result * 10) - t_int64(digit);
                end if;
                if is_negative and result > 0 then
                    -- Negate now to avoid problems around INTEGER'LOW
                    result := -result;
                    is_negative := false;
                end if;
                num_digits := num_digits + 1;
            end if;
            pos := pos + 1;
        end loop;

        assert num_digits > 0
            report "invalid integer value """ & s & """" severity failure;

        used := pos - 1;
        value := result;
    end procedure;

    function string_to_int (s : string) return t_int64 is
        variable used   : natural;
        variable value  : t_int64;
        constant length : natural := s'length;
        alias ss        : string(1 to length) is s;
    begin
        string_to_int(s, value, used);

        for i in used + 1 to length loop
            assert isspace(ss(i))
                report ("found invalid characters """ & ss(i to length)
                        & """ after value """ & s & """") severity failure;
        end loop;

        return value;
    end function;

    type t_real_array is array (natural range <>) of real;

    function string_to_real (s : string) return real is
        -- Derived from strtod implementation in FreeBSD
        constant length       : natural := s'length;
        alias ss              : string(1 to length) is s;
        variable pos          : positive := 1;
        constant max_exponent : integer := 511;
        constant powers_of_10 : t_real_array(0 to 8) := (
            10.0, 100.0, 1.0e4, 1.0e8, 1.0e16, 1.0e32, 1.0e64, 1.0e128, 1.0e256 );
        variable sign         : boolean;
        variable exp_sign     : boolean;
        variable fraction     : real;
        variable dbl_exp      : real;
        variable exp          : integer := 0;
        variable frac_exp     : integer := 0;
        variable mant_size    : integer := 0;
        variable dec_pt       : integer := -1;
        variable char         : character;
        variable frac1, frac2 : integer;
        constant ascii_zero   : natural := character'pos('0');
        variable p_exp        : positive;
    begin
        while pos <= length and isspace(ss(pos)) loop
            pos := pos + 1;
        end loop;

        if ss(pos) = '-' then
            sign := true;
            pos := pos + 1;
        elsif ss(pos) = '+' then
            pos := pos + 1;
        end if;

        -- Count the number of digits in the mantissa and locate the
        -- decimal point
        while pos <= length loop
            char := ss(pos);
            if not isdigit(char) then
                if char /= '.' or dec_pt >= 0 then
                    exit;
                end if;
                dec_pt := mant_size;
            end if;
            pos := pos + 1;
            mant_size := mant_size + 1;
        end loop;

        -- Collect the digits in the mantissa
        p_exp := pos;
        pos := pos - mant_size;
        if dec_pt < 0 then
            dec_pt := mant_size;
        else
            mant_size := mant_size - 1;
        end if;
        if mant_size > 18 then
            frac_exp := dec_pt - 18;
            mant_size := 18;
        else
            frac_exp := dec_pt - mant_size;
        end if;

        assert mant_size > 0
            report "zero sized mantissa: " & s severity failure;

        frac1 := 0;
        while mant_size > 9 loop
            char := ss(pos);
            pos := pos + 1;
            if char = '.' then
                char := ss(pos);
                pos := pos + 1;
            end if;
            frac1 := 10*frac1 + (character'pos(char) - ascii_zero);
            mant_size := mant_size - 1;
        end loop;
        frac2 := 0;
        while mant_size > 0 loop
            char := ss(pos);
            pos := pos + 1;
            if char = '.' then
                char := ss(pos);
                pos := pos + 1;
            end if;
            frac2 := 10*frac2 + (character'pos(char) - ascii_zero);
            mant_size := mant_size - 1;
        end loop;
        fraction := (1.0e9 * real(frac1)) + real(frac2);

        -- Skim off the exponent
        pos := p_exp;
        if pos <= length and (ss(pos) = 'E' or ss(pos) = 'e') then
            pos := pos + 1;
            if ss(pos) = '-' then
                exp_sign := true;
                pos := pos + 1;
            elsif ss(pos) = '+' then
                pos := pos + 1;
            end if;
            char := ss(pos);
            while pos <= length and isdigit(char) loop
                exp := exp * 10 + (character'pos(char) - ascii_zero);
                pos := pos + 1;
            end loop;
        end if;

        if exp_sign then
            exp := frac_exp - exp;
        else
            exp := frac_exp + exp;
        end if;

        -- Generate a floating-point number that represents the exponent
        if exp < 0 then
            exp_sign := true;
            exp := -exp;
        else
            exp_sign := false;
        end if;

        assert exp <= max_exponent
            report "exponent too large: " & s severity failure;

        dbl_exp := 1.0;
        for d in powers_of_10'range loop
            exit when exp = 0;
            if (exp rem 2) = 1 then
                dbl_exp := dbl_exp * powers_of_10(d);
            end if;
            exp := exp / 2;
        end loop;

        if exp_sign then
            fraction := fraction / dbl_exp;
        else
            fraction := fraction * dbl_exp;
        end if;

        for i in pos to length loop
            assert isspace(ss(i))
                report ("found invalid characters """ & ss(i to length)
                        & """ after value """ & s & """") severity failure;
        end loop;

        if sign then
            return -fraction;
        else
            return fraction;
        end if;
    end function;

    function change_bounds (s : string; l, r : positive) return string is
        alias ss : string(l to r) is s;
    begin
        return ss;
    end function;

    function int_to_string (x : t_int64) return string is
        variable tmp  : t_int64 := x;
        variable buf  : string(1 to 32);
        variable pos  : positive := buf'right;
        constant zero : natural := character'pos('0');
    begin
        loop
            buf(pos) := character'val(zero + integer(abs(tmp rem 10)));
            pos := pos - 1;
            tmp := tmp / 10;
            exit when tmp = 0;
        end loop;
        if x < 0 then
            buf(pos) := '-';
            pos := pos - 1;
        end if;
        return change_bounds(buf(pos + 1 to buf'right), 1, buf'right - pos);
    end function;

end package body;
