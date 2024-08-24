entity issue951 is
end entity;

architecture test of issue951 is
    type line is access string;
    type line_vector is array (natural range <>) of line;
    type lines_t is access line_vector;

    impure function split (
        constant s         : string;
        constant sep       : string;
        constant max_split : integer := -1) return lines_t
    is
        variable ret_val : lines_t;
    begin
        return ret_val;
    end split;

    type NaturalArray_t is array (natural range <>) of natural;

    impure function ParseIntStr(str : string) return NaturalArray_t is
        variable parts : lines_t := split(str, ", ");
        variable return_value : NaturalArray_t(parts'range);
    begin
        for i in parts'range loop       -- Crash here
            return_value(i) := integer'value(parts(i).all);
        end loop;
        return return_value;
    end;

begin
end architecture;
