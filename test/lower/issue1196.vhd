entity issue1196 is
end entity;

architecture test of issue1196 is
    subtype MemoryBaseType is integer;
    type MemBlockType      is array (integer range <>) of MemoryBaseType ;
    type MemBlockPtrType   is access MemBlockType ;
    type MemArrayType      is array (integer range <>) of MemBlockPtrType ;
    type MemArrayPtrType   is access MemArrayType ;
begin

    p1: process is
        variable ptr : MemArrayPtrType;
    begin
        ptr := new MemArrayType(0 to (2**30-1) + 2**30) ;
        wait;
    end process;

end architecture;
