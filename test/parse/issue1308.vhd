package pack is
  subtype RdyType is integer range  0 to integer'high ;
  subtype AckType is integer range -1 to integer'high ;

  type AddressBusRecType is record
    Rdy                : RdyType ;
    Ack                : AckType ;
  end record AddressBusRecType ;

  view AddressBusTestCtrlView of AddressBusRecType is
    Rdy                : out ;
    Ack                : in ;
  end view;

  type slv_vector is array (natural range <>) of bit_vector;

  ------------------------------------------------------------
  procedure ReadCheckBurstVector (
  ------------------------------------------------------------
    signal   TransactionRec : view AddressBusTestCtrlView of AddressBusRecType ;
             iAddr          : In    bit_vector ;
             VectorOfWords  : In    slv_vector ;
             StatusMsgOn    : In    boolean := false
  ) ;

  ------------------------------------------------------------
  procedure ReadCheckBurstVector (
  ------------------------------------------------------------
    signal   TransactionRec : view AddressBusTestCtrlView of AddressBusRecType ;
             iAddr          : In    bit_vector ;
             VectorOfWords  : In    integer_vector ;
             FifoWidth      : In    integer ;
             StatusMsgOn    : In    boolean := false
  ) ;
end package;

-------------------------------------------------------------------------------

entity issue1308 is
end entity;

use work.pack.all;

architecture test of issue1308 is
    constant DATA_ZERO  : bit_vector := (31 downto 0 => '0') ;
    signal ManagerRec : AddressBusRecType;
begin
    process is
    begin
        ReadCheckBurstVector(ManagerRec, X"0000_100A",
        (X"0001_00001", DATA_ZERO, DATA_ZERO) ) ;  -- OK

        wait;
    end process;
end architecture;
