-------------------------------------------------------------------------------
--  Copyright (C) 2014-2021  Nick Gasson
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Environment package for VHDL-2008
-------------------------------------------------------------------------------

package env is

    procedure stop(status : integer);
    procedure stop;

    procedure finish(status : integer);
    procedure finish;

    function resolution_limit return delay_length;

end package;
