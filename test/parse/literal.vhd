ARCHITECTURE a OF e IS
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
  CONSTANT k : ptr     := NULL;
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
BEGIN

END ARCHITECTURE;
