entity vhpi5 is
end entity;

architecture test of vhpi5 is
   type rec is record
      a, b: integer;
      c: integer_vector(0 to 1);
   end record;

   signal m: rec;
begin
end architecture;
