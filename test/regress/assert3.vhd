entity assert3 is
begin
    l : assert (false)
        report "should assert"
        severity note;
end entity assert3;

architecture a of assert3 is
begin
end architecture a;
