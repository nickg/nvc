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
            & " in string " & s severity failure;
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

end package body;
