-------------------------------------------------------------------------------
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

package body textbuf is

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

    impure function to_string (tb : inout text_buf_t) return string is
    begin
        return tb.buf(1 to tb.len);
    end function;

end package body;
