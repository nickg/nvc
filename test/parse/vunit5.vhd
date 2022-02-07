entity vunit5 is
    generic ( vunit5 : integer );       -- OK (hides entity)
end entity;

architecture test of vunit5 is
    signal x : integer := vunit5;       -- OK
begin
end architecture;
