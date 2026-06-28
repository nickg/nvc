package cfg_pkg is

    generic (
        g_data_width : positive
    );

    constant c_fixed_width : positive := 16;

    type aStream is record
        data  : bit_vector;
        ready : bit;
    end record;

    view aSinkGenericBroken of aStream(data(g_data_width - 1 downto 0)) is
        data  : in;
        ready : out;
    end view;

    subtype aStreamSubtype is aStream(data(g_data_width-1 downto 0));
    view aSinkGenericWorking of aStreamSubtype is
        data  : in;
        ready : out;
    end view;

    view aSinkConstant of aStream(data(c_fixed_width - 1 downto 0)) is
        data  : in;
        ready : out;
    end view;

end package cfg_pkg;

entity unit_ok is
    generic (
        package g_cfg_pkg is new work.cfg_pkg generic map (<>)
    );
    port (
        s_axis : view g_cfg_pkg.aSinkConstant
    );
end entity unit_ok;

architecture rtl of unit_ok is
begin

    s_axis.ready <= '0';

end architecture rtl;

entity unit_crash is
    generic (
        package g_cfg_pkg is new work.cfg_pkg generic map (<>)
    );
    port (
        s_axis : view g_cfg_pkg.aSinkGenericBroken
        -- s_axis : view g_cfg_pkg.aSinkGenericWorking
    );
end entity unit_crash;

architecture rtl of unit_crash is
begin

    s_axis.ready <= '0';

end architecture rtl;
