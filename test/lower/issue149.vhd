entity issue149 is
end entity;

architecture test of issue149 is

    procedure  PROC_NG(table: inout bit_vector) is
        variable i : integer range 0 to table'length-1;
    begin
        i := -1;
    end PROC_NG;

begin
end architecture;
