package sized_pkg is
    generic ( WIDTH : natural );

    --constant XWIDTH : natural := WIDTH;

    type t_vec is array (natural range <>) of bit_vector(WIDTH - 1 downto 0);
end package;

package settings_pkg is
    constant C_WIDTH : integer := 8;

    package sized8 is new work.sized_pkg generic map (C_WIDTH);
end package;

entity issue1422 is
end entity;

architecture test of issue1422 is
    signal s8 : work.settings_pkg.sized8.t_vec(1 to 2);
--    signal s9 : work.settings_pkg.sized8.t_vec'element;
begin
end architecture;
