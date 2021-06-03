--
-- Grab bag of miscellaneous VHDL-2008 syntax
--

entity vhdl2008 is
end entity;

architecture test of vhdl2008 is

    type my_utype is (a, b, c);
    type my_utype_vector is array (natural range <>) of my_utype;

    function resolved (s : my_utype_vector) return my_utype;

    subtype my_type is resolved my_utype;
    subtype my_type_vector is (resolved) my_utype_vector;  -- OK

begin

end architecture;
