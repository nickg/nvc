//
//  Copyright (C) 2023  Nick Gasson
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

import { formatTime } from "../lib/time-util";
import { expect, test } from "@jest/globals";

test("formatTime", () => {
  expect(formatTime(0n)).toBe("0 ns");
  expect(formatTime(1n)).toBe("1 fs");
  expect(formatTime(2500n)).toBe("2500 fs");
  expect(formatTime(2500000n)).toBe("2500 ps");
});
