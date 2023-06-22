entity vhpi5 is
end entity;

architecture test of vhpi5 is
   -- Simple record
   type rec is record
      a, b: integer;
      c: integer_vector(0 to 1);
   end record;

   function init_rec(base: integer) return rec is
      variable m: rec;
   begin
      m.a := (base + 0) * 16;
      m.b := (base + 1) * 16;
      for i in m.c'range loop
         m.c(i) := (base + 2) * 16 + i;
      end loop;
      return m;
   end;

   signal m: rec := init_rec(0);

   -- Array of records
   type rec_matrix is array(0 to 6, 0 to 4) of rec;

   function init_rec_matrix return rec_matrix is
      variable n: rec_matrix;
   begin
      for i in n'range(1) loop
         for j in n'range(2) loop
            n(i, j) := init_rec(i * 256 + j * 16);
         end loop;
      end loop;
      return n;
   end;

   signal n: rec_matrix := init_rec_matrix;

   -- Record with array of records
   type rec_vector is array(natural range <>) of rec;
   type rec2 is record
      a, b: integer;
      c, d: integer_vector(0 to 4);
      e, f, g: rec_vector(0 to 4);
   end record;

   function init_rec2 return rec2 is
      variable o: rec2;
   begin
      o.a := 0 * 4096;
      o.b := 1 * 4096;
      for i in o.c'range loop
         o.c(i) := 2 * 4096 + i * 256;
         o.d(i) := 3 * 4096 + i * 256;
      end loop;
      for i in o.e'range loop
         o.e(i) := init_rec(4 * 256 + i * 16);
         o.f(i) := init_rec(5 * 256 + i * 16);
         o.g(i) := init_rec(6 * 256 + i * 16);
      end loop;
      return o;
   end;

   signal o: rec2 := init_rec2;
begin
end architecture;
