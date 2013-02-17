--
-- TEXTIO package as defined by IEEE 1076-1993
--
package TEXTIO is

    type LINE is access STRING;

    type TEXT is file of STRING;

    type SIDE is (RIGHT, LEFT);

    subtype WIDTH is NATURAL;

    file INPUT : TEXT open READ_MODE is "STD_INPUT";

    file OUTPUT : TEXT open WRITE_MODE is "STD_OUTPUT";

    procedure READLINE (file F: TEXT; L: out LINE);

    procedure READ (L     : inout LINE;
                    VALUE : out BIT;
                    GOOD  : out BOOLEAN );
    procedure READ (L     : inout LINE;
                    VALUE : out BIT );

    procedure READ (L     : inout LINE;
                    VALUE : out BIT_VECTOR;
                    GOOD  : out BOOLEAN );
    procedure READ (L     : inout LINE;
                    VALUE : out BIT_VECTOR );

    procedure READ (L     : inout LINE;
                    VALUE : out BOOLEAN;
                    GOOD  : out BOOLEAN );
    procedure READ (L     : inout LINE;
                    VALUE : out BOOLEAN );

    procedure READ (L     : inout LINE;
                    VALUE : out CHARACTER;
                    GOOD  : out BOOLEAN );
    procedure READ (L     : inout LINE;
                    VALUE : out CHARACTER );

    procedure READ (L     : inout LINE;
                    VALUE : out INTEGER;
                    GOOD  : out BOOLEAN );
    procedure READ (L     : inout LINE;
                    VALUE : out INTEGER );

    procedure READ (L     : inout LINE;
                    VALUE : out REAL;
                    GOOD  : out BOOLEAN );
    procedure READ (L     : inout LINE;
                    VALUE : out REAL );

    procedure READ (L     : inout LINE;
                    VALUE : out STRING;
                    GOOD  : out BOOLEAN );
    procedure READ (L     : inout LINE;
                    VALUE : out STRING );

    procedure READ (L     : inout LINE;
                    VALUE : out TIME;
                    GOOD  : out BOOLEAN );
    procedure READ (L     : inout LINE;
                    VALUE : out TIME );

    procedure WRITELINE (file F : TEXT; L : inout LINE);

    procedure WRITE (L         : inout LINE;
                     VALUE     : in BIT;
                     JUSTIFIED : in SIDE:= RIGHT;
                     FIELD     : in WIDTH := 0 );

    procedure WRITE (L         : inout LINE;
                     VALUE     : in BIT_VECTOR;
                     JUSTIFIED : in SIDE:= RIGHT;
                     FIELD     : in WIDTH := 0 );

    procedure WRITE (L         : inout LINE;
                     VALUE     : in BOOLEAN;
                     JUSTIFIED : in SIDE:= RIGHT;
                     FIELD     : in WIDTH := 0 );

    procedure WRITE (L         : inout LINE;
                     VALUE     : in CHARACTER;
                     JUSTIFIED : in SIDE:= RIGHT;
                     FIELD     : in WIDTH := 0 );

    procedure WRITE (L         : inout LINE;
                     VALUE     : in INTEGER;
                     JUSTIFIED : in SIDE:= RIGHT;
                     FIELD     : in WIDTH := 0 );

    procedure WRITE (L         : inout LINE;
                     VALUE     : in REAL;
                     JUSTIFIED : in SIDE:= RIGHT;
                     FIELD     : in WIDTH := 0;
                     DIGITS    : in NATURAL:= 0 );

    procedure WRITE (L         : inout LINE;
                     VALUE     : in STRING;
                     JUSTIFIED : in SIDE:= RIGHT;
                     FIELD     : in WIDTH := 0 );

    procedure WRITE (L         : inout LINE;
                     VALUE     : in TIME;
                     JUSTIFIED : in SIDE:= RIGHT;
                     FIELD     : in WIDTH := 0;
                     UNIT      : in TIME:= ns );

end package;
