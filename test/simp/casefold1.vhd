-- Test case from Adam Barnes
--

package mycomp_pkg is

    type int_array is array(natural range <>) of integer;

    component mycomp is
        generic (
            mygen : int_array(1 to 6)
        );
        port (
            i : in  bit;
            o : out bit
        );
    end component mycomp;

end package mycomp_pkg;

--------------------------------------------------------------------------------

library work;
use work.mycomp_pkg.all;

entity mycomp is
    generic (
        mygen : int_array(1 to 6)
    );
    port (
        i : in  bit;
        o : out bit
    );
end entity mycomp;

architecture sim of mycomp is
begin

    o <= not i;

    casep: process
    begin
        --case mygen(1) is
        case 1 is
           when 0 to 128 => report "value of mygen(1) is in range 0..128; value is " & integer'image(mygen(1));
           when others  =>  report "value of mygen(1) is NOT in range 0..128; value is " & integer'image(mygen(1)) severity failure;
        end case;
        wait;
    end process;

end architecture;

entity top is
end entity top;

library work;
use work.mycomp_pkg.all;

architecture sim of top is

    signal test : bit;
    constant mygen : int_array := (0,1,2,3,4,5);

begin

    process
    begin
        case mygen(1) is
           when 0 to 128 => report "value of mygen(1) is in range 0..128; value is " & integer'image(mygen(1));
           when others  =>  report "value of mygen(1) is NOT in range 0..128; value is " & integer'image(mygen(1));
        end case;
        wait;
    end process;

    UUT: component mycomp
        generic map (
            mygen => mygen
        )
        port map (
            i => test,
            o => open
        );

end architecture sim;
