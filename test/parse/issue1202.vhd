package my_package is
    generic ( x : integer );
end package;

entity foo is
    generic (
        package pkg is work.my_package generic map (<>));
end entity;

architecture rtl of foo is
    use pkg.all;
begin

end architecture;
