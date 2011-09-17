--
-- STANDARD package as defined by IEEE 1076-1993
--
package STANDARD is
    
    type BOOLEAN is (FALSE, TRUE);

    type BIT is ('0', '1');

    type CHARACTER is (
        NUL, SOH, STX, ETX, EOT, ENQ, ACK, BEL, 
        BS,  HT,  LF,  VT,  FF,  CR,  SO,  SI, 
        DLE, DC1, DC2, DC3, DC4, NAK, SYN, ETB, 
        CAN, EM,  SUB, ESC, FSP, GSP, RSP, USP, 
        
        ' ', '!', '"', '#', '$', '%', '&', ''', 
        '(', ')', '*', '+', ',', '-', '.', '/', 
        '0', '1', '2', '3', '4', '5', '6', '7', 
        '8', '9', ':', ';', '<', '=', '>', '?', 

        '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 
        'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 
        'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 
        'X', 'Y', 'Z', '[', '\', ']', '^', '_', 

        '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 
        'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 
        'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 
        'x', 'y', 'z', '{', '|', '}', '~', DEL,

        C128, C129, C130, C131, C132, C133, C134, C135,
        C136, C137, C138, C139, C140, C141, C142, C143,
        C144, C145, C146, C147, C148, C149, C150, C151,
        C152, C153, C154, C155, C156, C157, C158, C159

        -- TODO: various extended characters
        );

    type SEVERITY_LEVEL is (NOTE, WARNING, ERROR, FAILURE);

    type INTEGER is range -2147483648 to 2147483647;
    
    --type REAL is range -1.0E307 to 1.0E307; 

    -- TODO: this needs to be 64 bit!
    type TIME is range -2147483648 to 2147483647
        units
            fs;
            ps  = 1000 fs;
            ns  = 1000 ps;
            us  = 1000 ns;
            ms  = 1000 us;
            sec = 1000 ms;
            min = 60 sec;
            hr  = 60 min;
        end units;

    --subtype DELAY_LENGTH is TIME range 0 fs to TIME'HIGH;

    -- XXX impure function NOW return DELAY_LENGTH;
    impure function NOW return TIME;

    --subtype NATURAL is INTEGER range 0 to INTEGER'HIGH;

    --subtype POSITIVE is INTEGER range 1 to INTEGER'HIGH;

    type STRING is array (POSITIVE range <>) of CHARACTER;

    type BIT_VECTOR is array (NATURAL range <>) of BIT;

    type FILE_OPEN_KIND is (READ_MODE,WRITE_MODE, APPEND_MODE);

    type FILE_OPEN_STATUS is (OPEN_OK, STATUS_ERROR, NAME_ERROR,
                              MODE_ERROR);

    --attribute FOREIGN : STRING;
    
end package;
