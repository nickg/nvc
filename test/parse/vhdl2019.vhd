-- LCS-2016-071a: Trailing semicolion
entity ent is
  port (
    a : in  boolean ;
    b : out boolean ;
  ) ;
end entity ;

-- LCS-2016-082: Empty record
package pack is

    type rec is record
    end record ;

end package ;

-- LCS-2016-086
entity E is
    generic (G1: INTEGER; G2: INTEGER := G1; G3, G4, G5, G6: INTEGER;G7:G6'SUBTYPE);
    port (P1: STRING(G3 to G4); P2: STRING(P1'RANGE); P3: P1'SUBTYPE);
    procedure X (Y1, Y2: INTEGER; Y3: INTEGER range Y1 to Y2; Y4: Y1'SUBTYPE);
end E;
