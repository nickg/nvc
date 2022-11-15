-- library foo

package pack is
    component comp is
        port ( p : in bit );
    end component;
end package;

-- library bar

library foo;

entity e is
end entity;

architecture a of e is
begin

    u: component foo.pack.comp     -- OK
        port map ( '1' );

end architecture;
