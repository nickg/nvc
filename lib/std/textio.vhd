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

-------------------------------------------------------------------------------
-- TEXTIO package as defined by IEEE 1076-1993
-------------------------------------------------------------------------------

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
                     justified : in side := right;
                     field     : in width := 0 );

    procedure write (l         : inout line;
                     value     : in bit_vector;
                     justified : in side := right;
                     field     : in width := 0 );

    procedure write (l         : inout line;
                     value     : in boolean;
                     justified : in side := right;
                     field     : in width := 0 );

    procedure write (l         : inout line;
                     value     : in character;
                     justified : in side := right;
                     field     : in width := 0 );

    procedure write (l         : inout line;
                     value     : in integer;
                     justified : in side := right;
                     field     : in width := 0 );

    procedure write (l         : inout line;
                     value     : in real;
                     justified : in side:= right;
                     field     : in width := 0;
                     digits    : in natural:= 0 );

    procedure write (l         : inout line;
                     value     : in string;
                     justified : in side := right;
                     field     : in width := 0 );

    procedure write (l         : inout line;
                     value     : in time;
                     justified : in side := right;
                     field     : in width := 0;
                     unit      : in time := ns );

    attribute never_waits : boolean;
    attribute never_waits of readline [text, line] : procedure is true;
    attribute never_waits of read [line, bit, boolean] : procedure is true;
    attribute never_waits of read [line, bit] : procedure is true;
    attribute never_waits of read [line, bit_vector, boolean] : procedure is true;
    attribute never_waits of read [line, bit_vector] : procedure is true;
    attribute never_waits of read [line, boolean, boolean] : procedure is true;
    attribute never_waits of read [line, boolean] : procedure is true;
    attribute never_waits of read [line, character, boolean] : procedure is true;
    attribute never_waits of read [line, character] : procedure is true;
    attribute never_waits of read [line, integer, boolean] : procedure is true;
    attribute never_waits of read [line, integer] : procedure is true;
    attribute never_waits of read [line, real, boolean] : procedure is true;
    attribute never_waits of read [line, real] : procedure is true;
    attribute never_waits of read [line, string, boolean] : procedure is true;
    attribute never_waits of read [line, string] : procedure is true;
    attribute never_waits of read [line, time, boolean] : procedure is true;
    attribute never_waits of read [line, time] : procedure is true;
    attribute never_waits of writeline [text, line] : procedure is true;
    attribute never_waits of write [line, bit, side, width] : procedure is true;
    attribute never_waits of write [line, bit_vector, side, width] : procedure is true;
    attribute never_waits of write [line, boolean, side, width] : procedure is true;
    attribute never_waits of write [line, character, side, width] : procedure is true;
    attribute never_waits of write [line, integer, side, width] : procedure is true;
    attribute never_waits of write [line, real, side, width, natural] : procedure is true;
    attribute never_waits of write [line, string, side, width] : procedure is true;
    attribute never_waits of write [line, time, side, width, time] : procedure is true;

end package;
