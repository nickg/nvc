entity record33 is
end entity;

architecture test of record33 is

    type t_list;
    type t_list_ptr is access t_list;

    type t_data is record
        msg : string;
    end record;

    type t_list is record
        seq   : natural;
        data  : t_data;
        chain : t_list_ptr;
    end record;

    type t_shared_queue is protected
        procedure push (data : in t_data);
        procedure pop (data : out t_data; seq : out natural);
    end protected;

    type t_shared_queue is protected body
        variable head, tail : t_list_ptr;
        variable next_seq   : natural;

        procedure push (data : in t_data) is
            variable elt : t_list_ptr;
        begin
            elt := new t_list'(seq => next_seq, data => data, chain => null);
            next_seq := next_seq + 1;
            if head = null then
                head := elt;
                tail := elt;
            else
                tail.chain := elt;
                tail := elt;
            end if;
        end procedure;

        procedure pop (data : out t_data; seq : out natural) is
        begin
            assert head /= null;
            data := head.data;
            seq := head.seq;
            head := head.chain;
        end procedure;
    end protected body;

    shared variable queue : t_shared_queue;
begin

    p1: process is
        variable data : t_data(msg(1 to 5));
        variable seq  : natural;
        variable msg  : string(1 to 5);
    begin
        data.msg := "hello";
        queue.push(data);
        data.msg := "world";
        queue.push(data);
        wait for 1 ns;

        queue.pop(data, seq);
        assert seq = 0;
        assert data.msg = "hello";

        queue.pop(data, seq);
        assert seq = 1;
        assert data.msg = "world";

        wait;
    end process;

end architecture;
