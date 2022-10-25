-- -*- coding: latin-1 -*-
entity ee is
end entity;

ARCHITECTURE aa OF ee IS
  SIGNAL pos : INTEGER := 64;
  SIGNAL neg : INTEGER := -265;
  CONSTANT c : INTEGER := 523;
  CONSTANT a : STRING  := "hel""lo";
  CONSTANT b : STRING  := """quote""";
  CONSTANT d : INTEGER := 1E3;          -- Integer not real
  CONSTANT e : REAL    := 1.234;
  CONSTANT f : REAL    := 0.21712;
  CONSTANT g : REAL    := 1.4e6;
  CONSTANT h : REAL    := 235.1e-2;
  CONSTANT i : INTEGER := 1_2_3_4;
  CONSTANT j : REAL    := 5_6_7.12_3;
  type ptr is access integer;
  shared variable k : ptr     := NULL;
  CONSTANT l : STRING  := "Setup time is too short";
  CONSTANT m : STRING  := "";
  CONSTANT n : STRING  := " ";
  CONSTANT o : STRING  := "A";
  CONSTANT p : STRING  := """";
  CONSTANT q : STRING  := %Setup time is too short%;
  CONSTANT r : STRING  := %%;
  CONSTANT s : STRING  := % %;
  CONSTANT t : STRING  := %A%;
  CONSTANT u : STRING  := %%%%;
  constant v : string  := "©";

  subtype lowercase is character range 'a' to 'z';
  type my_string is array (lowercase range <>) of character;
  constant w : my_string := "hello";

  constant too_big : integer := 9223372036854775808;  -- Error
  constant way_too_big : integer := 235423414124e124124;  -- Error
BEGIN

END ARCHITECTURE;
