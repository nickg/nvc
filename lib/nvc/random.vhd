-------------------------------------------------------------------------------
--  Copyright (C) 2025  Nick Gasson
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
-- Random number generation using MT19937 algorithm with optional command line
-- seed.
-------------------------------------------------------------------------------

package random is

    type t_uint32 is range 0 to 4294967295;

    impure function get_random return t_uint32;
    impure function get_random return boolean;

    attribute foreign of get_random [return t_uint32] :
        function is "INTERNAL _nvc_random_get_next";

end package;
