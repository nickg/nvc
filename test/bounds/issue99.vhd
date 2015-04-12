entity Top_PhysicalTest_Simple is
end entity;

architecture top of Top_PhysicalTest_Simple is
  type my_int is range 1 to 5;
  constant int_1     : INTEGER  := natural(0.5);   -- OK
  constant int_2     : INTEGER  := natural(-1.5);  -- Error
  constant int_3     : my_int   := my_int(integer'(-1));   -- Error
begin
end;
