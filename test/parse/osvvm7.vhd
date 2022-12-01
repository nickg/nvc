package ScoreboardGenericPkg1 is
  generic (
    type ExpectedType ;
    type ActualType
  ) ;

  type ScoreboardIdType is record
    Id : integer ;
  end record ScoreboardIdType ;

  impure function NewID (
    Name          : String
  ) return ScoreboardIDType ;

end ScoreboardGenericPkg1 ;

package body ScoreboardGenericPkg1 is

  impure function NewID (
    Name          : String
  ) return ScoreboardIDType is
  begin
    return (id => 0);
  end function NewID ;

end ScoreboardGenericPkg1 ;

-------------------------------------------------------------------------------

package ScoreBoardPkg_slv1 is new work.ScoreboardGenericPkg1
  generic map (
    ExpectedType        => bit_vector,
    ActualType          => bit_vector
  ) ;

-------------------------------------------------------------------------------

package ScoreBoardPkg_int1 is new work.ScoreboardGenericPkg1
  generic map (
    ExpectedType        => integer,
    ActualType          => integer
  ) ;

-------------------------------------------------------------------------------

use work.ScoreboardPkg_slv1.all ;
use work.ScoreboardPkg_int1.all ;

entity xMiiPhyRxTransmitter is
end entity xMiiPhyRxTransmitter ;
architecture behavioral of xMiiPhyRxTransmitter is

  signal DataFifo : work.ScoreboardPkg_slv1.ScoreboardIDType ;
  signal MetaFifo : work.ScoreboardPkg_int1.ScoreboardIDType ;

begin

  ------------------------------------------------------------
  --  Initialize alerts
  ------------------------------------------------------------
  Initialize : process
  begin
    DataFifo     <= NewID("DataFifo") ;
    MetaFifo     <= NewID("MetaFifo") ;
    wait ;
  end process Initialize ;

end architecture behavioral ;
