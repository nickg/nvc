entity mulphys is
end entity;

architecture test of mulphys is

    function multime(x : in integer) return time is
    begin
        return x * ns;
    end function;

begin

end architecture;
