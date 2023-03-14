entity array16 is
end entity;

architecture test of array16 is
    subtype MemoryBaseType is integer_vector;

    function InitMemoryBaseType(Size : integer) return integer_vector is
        constant BaseU : integer_vector(0 to Size-1)  := (others => -1) ;
    begin
        return BaseU ;
    end function InitMemoryBaseType ;

    type MemBlockType      is array (integer range <>) of MemoryBaseType ;

    function InitMemoryBlockType(BlockWidth, BaseWidth : integer) return MemBlockType is
        constant BaseU : MemoryBaseType(BaseWidth-1 downto 0) := InitMemoryBaseType(BaseWidth) ;
    begin
        return MemBlockType'(0 to 2**BlockWidth-1 => BaseU) ;
    end function InitMemoryBlockType ;

    signal s : MemBlockType(1 to 16)(1 to 3) := InitMemoryBlockType(4, 3);
begin

    p1: process is
    begin
        for i in s'range loop
            for j in s'element'range loop
                assert s(i)(j) = -1;
            end loop;
        end loop;
        wait;
    end process;

end architecture;
