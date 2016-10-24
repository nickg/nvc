PACKAGE bitstring IS

  CONSTANT x : t := X"1234";
  CONSTANT y : t := O"1234";
  CONSTANT z : t := X"ab";
  CONSTANT b : t := B"101";
  CONSTANT c : t := x"f";
  CONSTANT d : t := X"a_b";
  CONSTANT e : t := B"1111_1111_1111";
  CONSTANT f : t := X"FFF";
  CONSTANT g : t := O"777";
  CONSTANT h : t := X"777";
  CONSTANT i : t := B%1111_1111_1111%;
  CONSTANT j : t := X%FFF%;
  CONSTANT k : t := O%777%;
  CONSTANT l : t := X%777%;

END PACKAGE;

PACKAGE bitstring_error IS

  CONSTANT e1 : t := O"9";              -- Error

END PACKAGE;
