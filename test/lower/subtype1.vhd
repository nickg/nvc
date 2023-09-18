entity subtype1 is
end entity;

architecture test of subtype1 is

    function get_bv (r : natural) return bit_vector is
        subtype t_result is bit_vector(1 to r);

        function helper return bit_vector is
            variable x : t_result;
        begin
            return x;
        end function;
    begin
        return helper;
    end function;

begin

end architecture;
