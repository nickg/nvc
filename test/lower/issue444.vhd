package issue444 is
    type rec is record
        x : integer;
    end record;

    type rec_array is array (natural range <>) of rec;

    procedure proc (x : in integer);
end package;

package body issue444 is
    procedure proc (x : in integer) is
        -- This uses a temporary var to initialise the array...
        variable v : rec_array(1 to x) := (others => (x => x));

        -- ...which shouldn't be reused by 'VALUE and 'IMAGE here
        type t is (FOO, BAR);
    begin
    end procedure;
end package body;
