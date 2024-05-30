entity ee_ is end entity; -- ERROR expecting is (after ee)
entity e__e is end entity; -- ERROR expecting is (after e)
entity _e is end entity; -- ERROR expecting identifier (instead of _e)

entity e_e is end entity; -- OK
entity e_ee_e is end entity; -- OK

architecture aa_ of e_e is -- ERROR expecting of (after aa)
begin
end architecture;

architecture a__a of e_e is -- ERROR expecting of (after a)
begin
end architecture;

architecture a_a of e_e is -- OK
begin
end architecture;
