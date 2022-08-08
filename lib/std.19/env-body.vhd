-------------------------------------------------------------------------------
--  Copyright (C) 2014-2022  Nick Gasson
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
        report "not implemented" severity failure;
    end function;

    impure function gmtime return time_record is
    begin
        report "not implemented" severity failure;
    end function;

    impure function epoch return real is
    begin
        report "not implemented" severity failure;
    end function;

    function localtime (timer : real) return time_record is
    begin
        report "not implemented" severity failure;
    end function;

    function gmtime (timer : real) return time_record is
    begin
        report "not implemented" severity failure;
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
    begin
        report "not implemented" severity warning;
        return 0.0;
    end function;

    function seconds_to_time (real_val : in real) return time is
    begin
        report "not implemented" severity warning;
        return 0 fs;
    end function;

    function to_string (trec        : time_record;
                        frac_digits : integer range 0 to 6 := 0)
        return string is
    begin
        report "not implemented" severity warning;
        return "";
    end function;

    impure function getenv (name : string) return string is
        impure function impl (name : string) return string;
        attribute foreign of impl : function is "_std_env_getenv";
    begin
        return impl(name);
    end function;

    impure function getenv (name : string) return line is
    begin
        return new string'(getenv(name));
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
        return "";
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
    begin
        report "not implemented" severity failure;
    end function;

    impure function dir_itemisdir (path  : in string) return boolean is
    begin
        report "not implemented" severity failure;
    end function;

    impure function dir_itemisfile (path : in string) return boolean is
    begin
        report "not implemented" severity failure;
    end function;

    procedure dir_workingdir (path   : in string;
                              status : out dir_open_status) is
    begin
        report "not implemented" severity failure;
    end procedure;

    impure function dir_workingdir (path : in string)
        return dir_open_status is
    begin
        report "not implemented" severity failure;
    end function;

    impure function dir_workingdir return string is
    begin
        report "not implemented" severity failure;
    end function;

    procedure dir_createdir (path   : in string;
                             status : out dir_create_status) is
    begin
        report "not implemented" severity failure;
    end procedure;

    procedure dir_createdir (path    : in string;
                             parents : in boolean;
                             status  : out dir_create_status) is
    begin
        report "not implemented" severity failure;
    end procedure;

    impure function dir_createdir (path    : in string;
                                   parents : in boolean := false)
        return dir_create_status is
    begin
        report "not implemented" severity failure;
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
        report "not implemented" severity failure;
    end function;

    impure function to_string (variable call_path : inout call_path_vector;
                               separator : string := "" & lf)
        return string is
    begin
        report "not implemented" severity failure;
    end function;

    impure function to_string (variable call_path : inout call_path_vector_ptr; separator : string := "" & lf) return string is
    begin
        report "not implemented" severity failure;
    end function;

    impure function get_call_path return call_path_vector_ptr is
    begin
        report "not implemented" severity failure;
    end function;

    impure function file_name return line is
    begin
        report "not implemented" severity failure;
    end function;

    impure function file_name return string is
    begin
        report "not implemented" severity failure;
    end function;

    impure function file_path return line is
    begin
        report "not implemented" severity failure;
    end function;

    impure function file_path return string is
    begin
        report "not implemented" severity failure;
    end function;

    impure function file_line return positive is
    begin
        report "not implemented" severity failure;
    end function;

    impure function file_line return string is
    begin
        report "not implemented" severity failure;
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
