-- working library is lib1

--pkg_const.vhd
package pkg_const is
    constant my_pkg_const : natural := 8;
end package pkg_const;

--pkg_math.vhd
library work;                           -- Error here with preserve-case
    use work.pkg_const.all;

package pkg_math is
    constant my_math_const : natural := my_pkg_const;
end package pkg_math;
