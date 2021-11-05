-------------------------------------------------------------------------------
--  Copyright (C) 2012-2021  Nick Gasson
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU Lesser General Public License as
--  published by the Free Software Foundation; either version 3 of the
--  License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
--  You should have received a copy of the GNU Lesser General Public License
--  along with this program; if not, see <http://www.gnu.org/licenses/>.
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

end package;
