/* --------------------------------------------------------------------
 *
 * Copyright © 2008 by IEEE.
 *
 * This source file is an essential part of IEEE Std 1076-2008,
 * IEEE Standard VHDL Language Reference Manual. Verbatim copies of this
 * source file may be used and distributed without restriction.
 * Modifications to this source file as permitted in IEEE Std 1076-2008
 * may also be made and distributed. All other uses require permission
 * from the IEEE Standards Department(stds-ipr@ieee.org).
 * All other rights reserved.
 *
 * This source file is provided on an AS IS basis. The IEEE disclaims ANY
 * WARRANTY EXPRESS OR IMPLIED INCLUDING ANY WARRANTY OF MERCHANTABILITY
 * AND FITNESS FOR USE FOR A PARTICULAR PURPOSE. The user of the source file
 * shall indemnify and hold IEEE harmless from any damages or liability
 * arising out of the use thereof.
 *
 *   Title     :  vhpi_defs.c
 *             :
 *   Developers:  IEEE P1076 Working Group, VHPI Task Force
 *             :
 *   Purpose   :  This file contains utilities source code as examples
 *             :  of possible implementations.
 *             :
 * --------------------------------------------------------------------
 * modification history :
 * --------------------------------------------------------------------
 * $Revision: 1219 $
 * $Date: 2008-04-10 16:40:28 +0930 (Thu, 10 Apr 2008) $
 * --------------------------------------------------------------------
 */


/* utilities to print VHDL strings */

int vhpi_is_printable( unsigned char ch ) {

if (ch < 32) return 0;
if (ch < 127) return 1;
if (ch == 127) return 0;
if (ch < 160) return 0;
return 1;
}


static const char* VHPICharCodes[256] = {
  "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK",  "BEL" ,
  "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO" ,  "SI",
  "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN" , "ETB",
  "CAN", "EM",  "SUB", "ESC", "FSP", "GSP", "RSP" , "USP",
  " ", "!", "\"", "#", "$", "%", "&", "",
  "(", ")", "*", "+", ", ", "-", ".", "/",
  "0", "1", "2", "3", "4", "5", "6", "7",
  "8", "9", ":", ";", "<", "=", ">", "?",
  "@", "A", "B", "C", "D", "E", "F", "G",
  "H", "I", "J", "K", "L", "M", "N", "O",
  "P", "Q", "R", "S", "T", "U", "V", "W",
  "X", "Y", "Z", "[", "\\", "]", "^", "_",
  "`", "a", "b", "c", "d", "e", "f", "g",
  "h", "i", "j", "k", "l", "m", "n", "o",
  "p", "q", "r", "s", "t", "u", "v", "w",
  "x", "y", "z", "{", "|", "}", "~", "DEL",
  "C128", "C129", "C130", "C131", "C132", "C133", "C134", "C135",
  "C136", "C137", "C138", "C139", "C140", "C141", "C142", "C143",
  "C144", "C145", "C146", "C147", "C148", "C149", "C150", "C151",
  "C152", "C153", "C154", "C155", "C156", "C157", "C158", "C159",
  " ", "¡", "¢", "£", "¤", "¥", "¦", "§",
  "¨", "©", "ª", "«", "¬", "­", "®", "¯",
  "°", "±", "²", "³", "´", "µ", "¶", "·",
  "¸", "¹", "º", "»", "¼", "½", "¾", "¿",
  "À", "Á", "Â", "Ã", "Ä", "Å", "Æ", "Ç",
  "È", "É", "Ê", "Ë", "Ì", "Í", "Î", "Ï",
  "Ð", "Ñ", "Ò", "Ó", "Ô", "Õ", "Ö", "×",
  "Ø", "Ù", "Ú", "Û", "Ü", "Ý", "Þ", "ß",
  "à", "á", "â", "ã", "ä", "å", "æ", "ç",
  "è", "é", "ê", "ë", "ì", "í", "î", "ï",
  "ð", "ñ", "ò", "ó", "ô", "õ", "ö", "÷",
  "ø", "ù", "ú", "û", "ü", "ý", "þ", "ÿ" };

#define VHPI_GET_PRINTABLE_STRINGCODE( ch ) VHPICharCodes[unsigned char ch]
