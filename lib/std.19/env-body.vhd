-------------------------------------------------------------------------------
--  Copyright (C) 2022-2023  Nick Gasson
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
use nvc.textbuf.all;

package body env is

    -- Forward slash is a legal separator on both Windows and Unix
    constant DIR_SEPARATOR : string := "/";

    procedure stop_impl(finish, have_status : boolean; status : integer);

    attribute foreign of stop_impl : procedure is "_std_env_stop";

    procedure stop(status : integer) is
    begin
        stop_impl(finish => false, have_status => true, status => status);
    end procedure;

    procedure stop is
    begin
        stop_impl(finish => false, have_status => false, status => 0);
    end procedure;

    procedure finish(status : integer) is
    begin
        stop_impl(finish => true, have_status => true, status => status);
    end procedure;

    procedure finish is
    begin
        stop_impl(finish => true, have_status => false, status => 0);
    end procedure;

    function resolution_limit return delay_length is
    begin
        return fs;
    end function;

    impure function localtime return time_record is
    begin
        return localtime(epoch);
    end function;

    impure function gmtime return time_record is
    begin
        return gmtime(epoch);
    end function;

    impure function epoch return real is
        impure function impl return real;
        attribute foreign of impl : function is "_std_env_epoch";
    begin
        return impl;
    end function;

    function localtime (timer : real) return time_record is
        procedure impl (timer : real; tr : out time_record);
        attribute foreign of impl : procedure is "_std_env_localtime";
        variable result : time_record;
    begin
        impl(timer, result);
        return result;
    end function;

    function gmtime (timer : real) return time_record is
        procedure impl (timer : real; tr : out time_record);
        attribute foreign of impl : procedure is "_std_env_gmtime";
        variable result : time_record;
    begin
        impl(timer, result);
        return result;
    end function;

    function epoch (trec : time_record) return real is
    begin
        report "not implemented" severity failure;
    end function;

    function localtime (trec : time_record) return time_record is
    begin
        report "not implemented" severity failure;
    end function;

    function gmtime (trec : time_record) return time_record is
    begin
        report "not implemented" severity failure;
    end function;

    function "+" (trec  : time_record; delta : real) return time_record is
    begin
        report "not implemented" severity failure;
    end function;

    function "+" (delta : real; trec : time_record) return time_record is
    begin
        report "not implemented" severity failure;
    end function;

    function "-" (trec  : time_record; delta : real) return time_record is
    begin
        report "not implemented" severity failure;
    end function;

    function "-" (delta : real; trec : time_record) return time_record is
    begin
        report "not implemented" severity failure;
    end function;

    function "-" (tr1, tr2 : time_record) return real is
    begin
        report "not implemented" severity warning;
        return 0.0;
    end function;

    function time_to_seconds (time_val : in time) return real is
        variable frac, whole : integer;
    begin
        frac := (time_val mod sec) / fs;
        whole := time_val / sec;
        return real(whole) + real(frac) / 1.0e15;
    end function;

    function seconds_to_time (real_val : in real) return time is
        function impl (real_val : real) return time;
        attribute foreign of impl : function is "_std_env_seconds_to_time";
    begin
        return impl(real_val);
    end function;

    function to_string (trec        : time_record;
                        frac_digits : integer range 0 to 6 := 0)
        return string
    is
        function zero_pad (value, width : integer) return string is
            variable result : string(1 to width);
            variable temp   : integer := value;
        begin
            for i in width downto 1 loop
                result(i to i) := to_string(temp rem 10);
                temp := temp / 10;
            end loop;
            return result;
        end function;

        variable buf : string(1 to 26);
    begin
        buf(1 to 4) := zero_pad(trec.year, 4);
        buf(5) := '-';
        buf(6 to 7) := zero_pad(trec.month, 2);
        buf(8) := '-';
        buf(9 to 10) := zero_pad(trec.day, 2);
        buf(11) := 'T';
        buf(12 to 13) := zero_pad(trec.hour, 2);
        buf(14) := ':';
        buf(15 to 16) := zero_pad(trec.minute, 2);
        buf(17) := ':';
        buf(18 to 19) := zero_pad(trec.second, 2);
        if frac_digits > 0 then
            buf(20) := '.';
            buf(21 to 26) := zero_pad(trec.microsecond, 6);
            return buf(1 to 20 + frac_digits);
        else
            return buf(1 to 19);
        end if;
    end function;

    impure function getenv (name : string) return string is
        impure function impl (name : string) return string;
        attribute foreign of impl : function is "_std_env_getenv";
    begin
        return impl(name);
    end function;

    impure function getenv (name : string) return line is
        constant result : string := getenv(name);
    begin
        if result'length = 0 then
            return null;
        else
            return new string'(result);
        end if;
    end function;

    impure function vhdl_version return STRING is
        impure function impl return string;
        attribute foreign of impl : function is "_std_env_vhdl_version";
    begin
        return impl;
    end function;

    function tool_type return STRING is
    begin
        return "SIMULATION";
    end function;

    function tool_vendor return string is
    begin
        return "https://www.nickg.me.uk/nvc";
    end function;

    function tool_name return string is
    begin
        return "nvc";
    end function;

    function tool_edition return string is
    begin
        return "GPL";
    end function;

    function tool_version return string is
        function impl return string;
        attribute foreign of impl : function is "_std_env_tool_version";
    begin
        return impl;
    end function;

    procedure dir_open (dir    : out directory;
                        path   : in string;
                        status : out dir_open_status) is
    begin
        report "not implemented" severity failure;
    end procedure;

    impure function dir_open (dir  : out directory;
                              path : in string) return dir_open_status is
    begin
        report "not implemented" severity failure;
    end function;

    procedure dir_close (variable dir : inout directory) is
    begin
        report "not implemented" severity failure;
    end procedure;

    impure function dir_itemexists (path : in string) return boolean is
        impure function impl (path : in string) return boolean;
        attribute foreign of impl : function is "_std_env_itemexists";
    begin
        return impl(path);
    end function;

    impure function dir_itemisdir (path  : in string) return boolean is
        impure function impl (path : in string) return boolean;
        attribute foreign of impl : function is "_std_env_itemisdir";
    begin
        return impl(path);
    end function;

    impure function dir_itemisfile (path : in string) return boolean is
        impure function impl (path : in string) return boolean;
        attribute foreign of impl : function is "_std_env_itemisfile";
    begin
        return impl(path);
    end function;

    procedure dir_workingdir (path   : in string;
                              status : out dir_open_status) is
        procedure impl (path   : in string;
                        status : out dir_open_status);
        attribute foreign of impl : procedure is "_std_env_set_workingdir";
    begin
        impl(path, status);
    end procedure;

    impure function dir_workingdir (path : in string)
        return dir_open_status
    is
        variable status : dir_open_status;
    begin
        dir_workingdir(path, status);
        return status;
    end function;

    impure function dir_workingdir return string is
        impure function impl return string;
        attribute foreign of impl : function is "_std_env_get_workingdir";
    begin
        return impl;
    end function;

    procedure dir_createdir (path   : in string;
                             status : out dir_create_status) is
    begin
        dir_createdir(path, false, status);
    end procedure;

    procedure dir_createdir (path    : in string;
                             parents : in boolean;
                             status  : out dir_create_status) is
        procedure impl (path    : in string;
                        parents : in boolean;
                        status  : out dir_create_status);
        attribute foreign of impl : procedure is "_std_env_createdir";
    begin
        assert not parents report "PARENTS flag not supported"
            severity failure;
        impl(path, parents, status);
    end procedure;

    impure function dir_createdir (path    : in string;
                                   parents : in boolean := false)
        return dir_create_status
    is
        variable status : dir_create_status;
    begin
        dir_createdir(path, parents, status);
        return status;
    end function;

    procedure dir_deletedir (path   : in string;
                             status : out dir_delete_status) is
    begin
        report "not implemented" severity failure;
    end procedure;

    procedure dir_deletedir (path      : in string;
                             recursive : in boolean;
                             status    : out dir_delete_status) is
    begin
        report "not implemented" severity failure;
    end procedure;

    impure function dir_deletedir (path      : in string;
                                   recursive : in boolean := false)
        return dir_delete_status is
    begin
        report "not implemented" severity failure;
    end function;

    procedure dir_deletefile (path   : in string;
                              status : out file_delete_status) is
    begin
        report "not implemented" severity failure;
    end procedure;

    impure function dir_deletefile (path : in string) return file_delete_status is
    begin
        report "not implemented" severity failure;
    end function;

    impure function to_string (variable call_path : inout call_path_element)
        return string is
    begin
        return call_path.file_path.all & DIR_SEPARATOR & call_path.file_name.all
            & ":" & to_string(call_path.file_line) & ":" & call_path.name.all;
    end function;

    impure function to_string (variable call_path : inout call_path_vector;
                               separator : string := "" & lf) return string is
        variable tb : text_buf_t;
    begin
        if call_path'length > 0 then
            for i in call_path'range loop
                if i > call_path'left then
                    tb_cat(tb, separator);
                end if;
                tb_cat(tb, to_string(call_path(i)));
            end loop;
            return to_string(tb);
        else
            return "";
        end if;
    end function;

    impure function to_string (variable call_path : inout call_path_vector_ptr;
                               separator : string := "" & lf) return string is
    begin
        if call_path /= null then
            return to_string(call_path.all);
        else
            return "";
        end if;
    end function;

    impure function get_call_path return call_path_vector_ptr is
        procedure impl (ptr : out call_path_vector_ptr);
        attribute foreign of impl : procedure is "_std_env_get_call_path";
        variable result : call_path_vector_ptr;
    begin
        impl(result);
        return result;
    end function;

    impure function file_name return line is
        procedure impl (ptr : out line);
        attribute foreign of impl : procedure is "_std_env_file_name";
        variable result : line;
    begin
        impl(result);
        return result;
    end function;

    impure function file_name return string is
        procedure impl (ptr : out line);
        attribute foreign of impl : procedure is "_std_env_file_name";
        variable result : line;
    begin
        impl(result);
        return result.all;
    end function;

    impure function file_path return line is
        procedure impl (ptr : out line);
        attribute foreign of impl : procedure is "_std_env_file_path";
        variable result : line;
    begin
        impl(result);
        return result;
    end function;

    impure function file_path return string is
        procedure impl (ptr : out line);
        attribute foreign of impl : procedure is "_std_env_file_path";
        variable result : line;
    begin
        impl(result);
        return result.all;
    end function;

    impure function file_line return positive is
        impure function impl return positive;
        attribute foreign of impl : function is "_std_env_file_line";
    begin
        return impl;
    end function;

    impure function file_line return string is
        impure function impl return positive;
        attribute foreign of impl : function is "_std_env_file_line";
    begin
        return to_string(impl);
    end function;

    impure function IsVhdlAssertFailed return boolean is
    begin
        report "not implemented" severity failure;
    end function;

    impure function IsVhdlAssertFailed (Level : severity_level)
        return boolean is
    begin
        report "not implemented" severity failure;
    end function;

    impure function GetVhdlAssertCount return natural is
    begin
        report "not implemented" severity failure;
    end function;

    impure function GetVhdlAssertCount (Level : severity_level)
        return natural is
    begin
        report "not implemented" severity failure;
    end function;

    procedure ClearVhdlAssert is
    begin
        report "not implemented" severity failure;
    end procedure;

    procedure SetVhdlAssertEnable (Enable : boolean := true) is
    begin
        report "not implemented" severity failure;
    end procedure;

    procedure SetVhdlAssertEnable (Level  : severity_level := note;
                                   Enable : boolean := true) is
    begin
        report "not implemented" severity failure;
    end procedure;

    impure function GetVhdlAssertEnable (Level : severity_level := note)
        return boolean is
    begin
        report "not implemented" severity failure;
    end function;

    procedure SetVhdlAssertFormat(Level  : SEVERITY_LEVEL;
                                  Format : string) is
    begin
        report "not implemented" severity failure;
    end procedure;

    procedure SetVhdlAssertFormat (Level  : severity_level;
                                   Format : string;
                                   Valid  : out boolean) is
    begin
        report "not implemented" severity failure;
    end procedure;

    impure function GetVhdlAssertFormat (Level : severity_level)
        return string is
    begin
        report "not implemented" severity failure;
    end function;

    procedure SetVhdlReadSeverity (Level : severity_level := failure) is
    begin
        report "not implemented" severity failure;
    end procedure;

    impure function GetVhdlReadSeverity return severity_level is
    begin
        report "not implemented" severity failure;
    end function;

    impure function PslAssertFailed return boolean is
    begin
        report "not implemented" severity failure;
    end function;

    impure function PslIsCovered return boolean is
    begin
        report "not implemented" severity failure;
    end function;

    procedure SetPslCoverAssert (Enable : boolean := TRUE) is
    begin
        report "not implemented" severity warning;
    end procedure;

    impure function GetPslCoverAssert return boolean is
    begin
        report "not implemented" severity warning;
        return false;
    end function;

    impure function PslIsAssertCovered return boolean is
    begin
        report "not implemented" severity warning;
        return false;
    end function;

    procedure ClearPslState is
    begin
        report "not implemented" severity warning;
    end procedure;

end package body;
