entity vhpi5 is
end entity;

architecture test of vhpi5 is
   -- Simple record
   type rec is record
      a, b: integer;
      c: integer_vector(0 to 1);
   end record;

   signal m: rec;

   -- Array of records
   type rec_matrix is array(0 to 6, 0 to 4) of rec;

   signal n: rec_matrix;

   -- Record with array of records
   type rec_vector is array(natural range <>) of rec;
   type rec2 is record
      a, b: integer;
      c, d: integer_vector(0 to 4);
      e, f, g: rec_vector(0 to 4);
   end record;

   signal o: rec2;
begin
end architecture;
