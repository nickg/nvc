entity issueA is
    generic (
        L : integer range 0 to integer'high
    );
begin
end entity issueA;

architecture a of issueA is
    pure function fL (
        iDUMMY : boolean
    ) return integer is
    begin
        if (L < 1) then
            return 1;
        else
            return L;
        end if;
    end function fL;
    constant cL : integer range 1 to integer'high := fL(true);
begin
end architecture a;
