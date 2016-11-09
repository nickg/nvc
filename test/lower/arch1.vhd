entity arch1 is
    generic ( sld_node_n_scan : integer );
end entity;

architecture test of arch1 is
    type rec is record
        x, y : integer;
        z : bit_vector(3 downto 0);
    end record;
    type scansArray is array (sld_node_n_scan - 1 downto 0) of rec;
    type rec2 is record
        r : scansArray;
    end record;

    function f return scansArray is
        variable v : rec2;
    begin
        return v.r;
    end function;

    function g return integer is
    begin
        return 5;
    end function;
begin

end architecture;
