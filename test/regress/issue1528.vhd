package parent_pkg is
    generic(
        type t;
    );
end package parent_pkg;

package child_pkg is
    generic(
        type t;
        package parent is new work.parent_pkg
            generic map(<>);
        pure function to_string(constant data : t) return string;
    );
    procedure print(constant data : in t);
end package child_pkg;

package body child_pkg is
    procedure print(constant data : in t) is
    begin
        report to_string(data);
    end procedure print;
end package body child_pkg;

entity dummy is
    generic(
        package child_pkg_instance is new work.child_pkg
            generic map(<>);
    );
    port(
        reg : out child_pkg_instance.parent.t -- Problem occurs here
    );
end entity dummy;
architecture dummy of dummy is
begin
end architecture dummy;


--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

entity issue1528 is
end entity;

architecture sim of issue1528 is
    package parent_pkg_integer is new work.parent_pkg
        generic map(
            t => integer
        );
    package child_pkg_integer is new work.child_pkg
        generic map(
            t         => parent_pkg_integer.t,
            parent    => parent_pkg_integer,
            to_string => to_string
        );
begin
    inst : entity work.dummy
        generic map(
            child_pkg_instance => child_pkg_integer
        )
        port map(
            reg => open
        );

    main : process is
    begin
      child_pkg_integer.print(parent_pkg_integer.t'(1));
      wait;
    end process main;
end architecture sim;
