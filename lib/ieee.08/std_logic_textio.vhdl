-- -----------------------------------------------------------------
-- 
-- Copyright 2019 IEEE P1076 WG Authors
-- 
-- See the LICENSE file distributed with this work for copyright and
-- licensing information and the AUTHORS file.
-- 
-- This file to you under the Apache License, Version 2.0 (the "License").
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied.  See the License for the specific language governing
-- permissions and limitations under the License.
--
--   Title     :  Standard multivalue logic package
--             :  (STD_LOGIC_TEXTIO package declaration)
--             :
--   Library   :  This package shall be compiled into a library
--             :  symbolically named IEEE.
--             :
--   Developers:  Accellera VHDL-TC and IEEE P1076 Working Group
--             :
--   Purpose   :  This packages is provided as a replacement for non-standard
--             :  implementations of the package provided by implementers of
--             :  previous versions of this standard. The declarations that
--             :  appeared in those non-standard implementations appear in the
--             :  package STD_LOGIC_1164 in this standard.
--             :
--   Note      :  No declarations or definitions shall be included in,
--             :  or excluded from this package.
--             :
-- --------------------------------------------------------------------
-- $Revision: 1305 $
-- $Date: 2008-06-27 14:28:49 +0930 (Fri, 27 Jun 2008) $
-- --------------------------------------------------------------------

use STD.TEXTIO.all;
library IEEE;
use IEEE.std_logic_1164.all;

PACKAGE std_logic_textio IS

  alias READ  is IEEE.std_logic_1164.READ [LINE, STD_ULOGIC];
  alias READ  is IEEE.std_logic_1164.READ [LINE, STD_ULOGIC, BOOLEAN];
  alias READ  is IEEE.std_logic_1164.READ [LINE, STD_ULOGIC_VECTOR];
  alias READ  is IEEE.std_logic_1164.READ [LINE, STD_ULOGIC_VECTOR, BOOLEAN];
  alias WRITE is IEEE.std_logic_1164.WRITE [LINE, STD_ULOGIC, SIDE, WIDTH];
  alias WRITE is IEEE.std_logic_1164.WRITE [LINE, STD_ULOGIC_VECTOR, SIDE, WIDTH];

  alias HREAD  is IEEE.std_logic_1164.HREAD [LINE, STD_ULOGIC_VECTOR];
  alias HREAD  is IEEE.std_logic_1164.HREAD [LINE, STD_ULOGIC_VECTOR, BOOLEAN];
  alias HWRITE is IEEE.std_logic_1164.HWRITE [LINE, STD_ULOGIC_VECTOR, SIDE, WIDTH];

  alias OREAD  is IEEE.std_logic_1164.OREAD [LINE, STD_ULOGIC_VECTOR];
  alias OREAD  is IEEE.std_logic_1164.OREAD [LINE, STD_ULOGIC_VECTOR, BOOLEAN];
  alias OWRITE is IEEE.std_logic_1164.OWRITE [LINE, STD_ULOGIC_VECTOR, SIDE, WIDTH];

END PACKAGE std_logic_textio;
