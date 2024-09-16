entity array_issue is
end entity array_issue;

architecture rtl of array_issue is
    constant C_LENGTH : positive := 7;
    type t_array1 is array (C_LENGTH downto 0) of bit;   -- OK
    type t_array2 is array (0 to C_LENGTH) of bit;  -- OK
    type t_array3 is array (positive range 0 to C_LENGTH) of bit;  -- Error
    type t_array4 is array (positive range C_LENGTH downto 0) of bit;  -- Error
begin
end architecture rtl;
