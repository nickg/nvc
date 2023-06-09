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

import Trace from "../lib/trace";
import { DataType } from "../lib/trace";
import { expect, test } from "@jest/globals";

test("data types", () => {
  expect(new Trace("l1").dataType).toBe(DataType.STD_LOGIC);
  expect(new Trace("eFOO").dataType).toBe(DataType.GENERIC);
});

test("value at time", () => {
  const t = new Trace("l1");
  t.update(0n, "l1");
  t.update(1n, "l0");
  t.update(5n, "l1");

  expect(t.valueAt(0n)).toBe("1");
  expect(t.valueAt(3n)).toBe("0");
  expect(t.valueAt(500n)).toBe("1");
});

test("mapWaveform basic", () => {
  const t = new Trace("eONE");
  t.update(0n, "eTWO");
  t.update(1n, "eTHREE");
  t.update(5n, "eFOUR");
  t.update(7n, "eFIVE");
  t.update(10n, "eSIX");

  const collect = (start: bigint, end: bigint) => {
    const result: object[] = [];
    t.mapWaveform(start, end, (s, e, v) => {
      result.push([s, e, v]);
    });
    return result;
  };

  expect(collect(0n, 2n)).toStrictEqual(
    [[0n, 1n, "TWO"], [1n, 2n, "THREE"]]);

  expect(collect(8n, 12n)).toStrictEqual(
    [[7n, 10n, "FIVE"], [10n, 12n, "SIX"]]);
});
