-- LCS-2016-006f
package pack006f is

    procedure info ;
end package;

package body pack006f is
    use std.env.all ;

    procedure info is
    begin
        report VHDL_VERSION ;
        report TOOL_TYPE ;
        report TOOL_VENDOR ;
        report TOOL_NAME ;
        report TOOL_EDITION ;
        report TOOL_VERSION ;
    end procedure ;

end package body ;

-- LCS-2016-055a: Syntax Regularization for endings
package pack055a is

    component silly is
      port (
        a : bit_vector
      ) ;
    end ;

end package ;

-- LCS-2016-071a: Trailing semicolion
entity ent is
  port (
    a : in  boolean ;
    b : out boolean ;
  ) ;
end entity ;

-- LCS-2016-072b: Function Knows Vector Size
package pack072b is
    function to_bitvector(x : natural) return rv_t of bit_vector ;

end package ;

package body pack072b is

    function to_bitvector(x : natural) return rv_t of bit_vector is
        variable rv : rv_t := (others =>'0') ;
        variable leftover : natural := x ;
    begin
        assert x < 2**rv'length
          report "overflow"
          severity warning ;
        for pow in 0 to rv'high loop
            if leftover mod 2 = 1 then
                rv(pow) := '1' ;
            end if ;
            leftover := leftover / 2 ;
        end loop ;
        return rv;
    end function ;

end package body;

-- LCS-2016-082: Empty record
package pack082 is
    type rec is record
    end record ;
end package;

-- LCS-2016-086
entity E is
    generic (G1: INTEGER; G2: INTEGER := G1; G3, G4, G5, G6: INTEGER;G7:G6'SUBTYPE);
    port (P1: STRING(G3 to G4); P2: STRING(P1'RANGE); P3: P1'SUBTYPE);
    procedure X (Y1, Y2: INTEGER; Y3: INTEGER range Y1 to Y2; Y4: Y1'SUBTYPE);
end E;

-- LCS-2016-059
entity E is
    generic (
        type t1 is private;
        type t2 is <>;
        type t3 is (<>);
        type t4 is range <>;
        type t5 is units <>;
        type t6 is range <> . <>;
        type t7 is array (t3 range <>) of t1;
        type t8 is access type is private;
        type t9 is file of t1;
        );
end E;

-- LCS-2015-016
entity E is
    port ( p : type is private );
end entity;
