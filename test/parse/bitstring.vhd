PACKAGE bitstring IS

  CONSTANT x : bit_vector := X"1234";
  CONSTANT y : bit_vector := O"1234";
  CONSTANT z : bit_vector := X"ab";
  CONSTANT b : bit_vector := B"101";
  CONSTANT c : bit_vector := x"f";
  CONSTANT d : bit_vector := X"a_b";
  CONSTANT e : bit_vector := B"1111_1111_1111";
  CONSTANT f : bit_vector := X"FFF";
  CONSTANT g : bit_vector := O"777";
  CONSTANT h : bit_vector := X"777";
  CONSTANT i : bit_vector := B%1111_1111_1111%;
  CONSTANT j : bit_vector := X%FFF%;
  CONSTANT k : bit_vector := O%777%;
  CONSTANT l : bit_vector := X%777%;

END PACKAGE;

PACKAGE bitstring_error IS

  CONSTANT e1 : bit_vector := O"9";              -- Error

END PACKAGE;
