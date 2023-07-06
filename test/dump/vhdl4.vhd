entity E is
    generic (
        type t1 is private;
        type t2 is <>;
        type t3 is (<>);
        type t4 is range <>;
        type t5 is units <>;
        type t6 is range <> . <>;
        type t7 is array (t3 range <>) of t1;
        type t8 is access type is private;
        type t9 is file of t1;
        );
end E;
