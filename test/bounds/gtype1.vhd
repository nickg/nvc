package test_pkg is
  generic (
    type A is array(type is range <>) of type is private ;
    function "&"(L : A'element ; R : A) return A is <> ;
    function "+"(L : A'index ; R : integer) return A'index is <>
   ) ;

    function create_rt(size : natural ; init : A'element) return A ;

    function create_recurse(size : natural ; init : A'element) return A ;

end package ;

package body test_pkg is

    -- Doesn't work for indices which have a range of exactly 1 element
    constant NULL_LOW : A'index := A'index'low when A'index'high - A'index'low > 0 else A'index'high ;
    constant NULL_HIGH : A'index := A'index'high when A'index'high - A'index'low > 0 else A'index'low ;
    subtype NULL_RANGE is A'index range NULL_HIGH to NULL_LOW ;

    function create_rt(size : natural ; init : A'element) return A is
        constant rv : A(A'index'low to A'index'low+size) := (others => init) ;
        variable nullarray : A(NULL_RANGE) ;
    begin
        return nullarray when size = 0 else rv ;
    end function ;

    function create_recurse(size : natural ; init : A'element) return A is
        variable nullarray : A(NULL_RANGE) ;
    begin
        return nullarray when size = 0 else
               A'(A'index'low => init) when size = 1 else
               init & create_recurse(size-1, init) ;
    end function ;

end package body ;

entity test is
end entity ;

architecture arch of test is

    subtype myint is integer range 25 to 40 ;
    type myarray is array(myint range <>) of real ;

    subtype nullint is integer range 40 to 25 ;
    type nullarray is array(nullint range <>) of real ;

    subtype singleint is integer range 42 downto 42 ;
    type singlearray is array(singleint range <>) of real ;

    package mytest_pkg is new work.test_pkg generic map(A => myarray) ;
    package nulltest_pkg is new work.test_pkg generic map (A => nullarray) ;
    package singletest_pkg is new work.test_pkg generic map (A => singlearray) ;

begin
end architecture ;
