Library ieee;
use ieee.std_logic_1164.all;

entity parse3 is
end entity;

architecture test of parse3 is
    signal a, b, c, clk : bit;
    signal global_int : integer;
    signal global_vect : bit_vector(3 downto 0);
    signal global_string : string(1 to 10);
    signal global_bool : boolean;
begin

    -- psl default clock is clk'event and clk = '1';

    -- psl assert never b;
    -- psl assert always (a -> next_a[3 to 5] (b));
    -- psl assert always (a -> next[3] (b));
    -- psl assert always (a -> next_event(b)[4](c));
    -- psl assert {a};
    -- psl assert {a;b and c};

    -- psl cover {a ;  b;c   };
    -- psl named_cover   : cover {c} report "'c' is covered";

    -- psl assume b;
    -- psl named_assume     : assume (a    -> b);
    -- psl assume_guarantee ( b ->    c) report "assume_guarantee met";

    -- psl restrict {c ;b ;   c;   b};
    -- psl named_restrict : restrict {c};
    -- psl restrict_guarantee   {b; b} report "restrict_guarantee met";

    -- psl fairness (b = '1');
    -- psl named_fairness : fairness (a = '1');
    -- psl strong fairness (a = '1'), (b = '1');

    -- psl property my_prop_1 is (a='1');
    -- psl sequence my_seq_1 is {b};


    -- psl property p_const_1(const i) is {global_int=i};
    -- psl property p_const_2(const numeric i) is {global_int=i};

    -- psl property p_const_numeric(const numeric i) is {global_int=i};
    -- psl property p_const_bit(const bit i) is {a=i};
    -- psl property p_const_bitvector(const bitvector i) is {global_vect=i};
    -- psl property p_const_string(const string i) is {global_string=i};
    -- psl property p_const_boolean(const boolean i) is {global_bool=i};

    -- psl property p_mutable_numeric(mutable numeric i) is {global_int=i};
    -- psl property p_mutable_bit(mutable bit i) is {a=i};
    -- psl property p_mutable_bitvector(mutable bitvector i) is {global_vect=i};
    -- psl property p_mutable_string(mutable string i) is {global_string=i};
    -- psl property p_mutable_boolean(mutable boolean i) is {global_bool=i};

    -- psl property p_hdltype_1(hdltype integer i) is {a};
    -- psl property p_hdltype_2(hdltype std_logic_vector i) is {a};
    -- psl property p_hdltype_3(hdltype std_logic_vector(5 downto 0) i) is {a};
    -- psl property p_hdltype_4(const hdltype integer i; hdltype integer j) is {global_int=(i+j)};
    -- psl property p_hdltype_5(const hdltype integer i; mutable hdltype integer j) is {global_int=(i+j)};

    -- psl property p_property_1(property x) is {a};
    -- psl property p_sequence_1(sequence x) is {a};

end architecture;
