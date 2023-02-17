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
