entity vhpi7 is
end entity;

architecture test of vhpi7 is
   type s is array(0 to 1) of integer;
   type r is array(0 to 2) of s;

   function init_r return r is
      variable m: r;
   begin
      for i in m'range loop
         for j in m(i)'range loop
            m(i)(j) := i * 16 + j;
         end loop;
      end loop;
      return m;
   end;

   signal m: r := init_r;

   type r2 is array(0 to 2, 0 to 1) of integer;
   type h2 is array(0 to 6, 0 to 4) of r2;

   function init_h2 return h2 is
      variable n: h2;
   begin
      for i in n'range(1) loop
         for j in n'range(2) loop
            for k in n(i, j)'range(1) loop
               for l in n(i, j)'range(2) loop
                  n(i, j)(k, l) := i * 4096 + j * 256 + k * 16 + l;
               end loop;
            end loop;
         end loop;
      end loop;
      return n;
   end;

   signal n: h2 := init_h2;

   type su is array(natural range <>) of integer;
   type ru is array(natural range <>) of su;

   function init_ru return ru is
      variable o: ru(0 to 2)(0 to 1);
   begin
      for i in o'range loop
         for j in o(i)'range loop
            o(i)(j) := i * 16 + j;
         end loop;
      end loop;
      return o;
   end;

   signal o: ru(0 to 2)(0 to 1) := init_ru;
begin
end architecture;
