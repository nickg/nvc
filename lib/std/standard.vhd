-- -------------------------------------------------*- coding: latin-1; -*-----
--  Copyright (C) 2011-2021  Nick Gasson
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
-- STANDARD package as defined by IEEE 1076-1993.
-------------------------------------------------------------------------------

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
        C152, C153, C154, C155, C156, C157, C158, C159,

        ' ', '¡', '¢', '£', '¤', '¥', '¦', '§',
        '¨', '©', 'ª', '«', '¬', '­', '®', '¯',
        '°', '±', '²', '³', '´', 'µ', '¶', '¹',
        C184, C185, C186, C187, C188, C189, C190, C191,

        C192, C193, C194, C195, C196, C197, C198, C199,
        C200, C201, C202, C203, C204, C205, C206, C207,
        C208, C209, C210, C211, C212, C213, C214, C215,
        C216, C217, C218, C219, C220, C221, C222, C223,

        C224, C225, C226, C227, C228, C229, C230, C231,
        C232, C233, C234, C235, C236, C237, C238, C239,
        C240, C241, C242, C243, C244, C245, C246, C247,
        C248, C249, C250, C251, C252, C253, C254, C255
        );

    type SEVERITY_LEVEL is (NOTE, WARNING, ERROR, FAILURE);

    -- type universal_integer is range implementation_defined;

    type INTEGER is range -2147483648 to 2147483647;

    -- type universal_real is range implementation_defined;

    type REAL is range -1.7976931348623157e308 to 1.7976931348623157e308;

    type TIME is range -9223372036854775808 to 9223372036854775807
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

    subtype DELAY_LENGTH is TIME range 0 fs to TIME'HIGH;

    impure function NOW return DELAY_LENGTH;

    subtype NATURAL is INTEGER range 0 to INTEGER'HIGH;

    subtype POSITIVE is INTEGER range 1 to INTEGER'HIGH;

    type STRING is array (POSITIVE range <>) of CHARACTER;

    type BIT_VECTOR is array (NATURAL range <>) of BIT;

    type FILE_OPEN_KIND is (READ_MODE, WRITE_MODE, APPEND_MODE);

    type FILE_OPEN_STATUS is (OPEN_OK, STATUS_ERROR, NAME_ERROR,
                              MODE_ERROR);

    attribute FOREIGN : STRING;

    attribute FOREIGN of NOW : function is "_std_standard_now";

end package;
