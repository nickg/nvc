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

type TraceValue = number | string;

export enum DataType {
  GENERIC,
  STD_LOGIC,
  BIT,
}

export interface IWaveMap {
  (start: bigint, end: bigint, value: string, index: number): void;
}

class Trace {
  private times: bigint[] = [];
  private values: TraceValue[] = [];
  private _dataType: DataType;

  constructor(value: string) {
    if (value[0] == "l")
      this._dataType = DataType.STD_LOGIC;
    else if (value[0] == "b")
      this._dataType = DataType.BIT;
    else
      this._dataType = DataType.GENERIC;

    this.update(0n, value);
  }

  get dataType() {
    return this._dataType;
  }

  update(when: bigint, value: string) {
    const count = this.times.length;
    const tail = value.slice(1);

    if (count > 0 && this.times[count - 1] == when) {
      this.values[count - 1] = tail;
    }
    else {
      this.times.push(when);
      this.values.push(tail);
    }
  }

  private findUpdateBefore(start: bigint): number {
    const max = this.times.length;

    // TODO: binary search...
    for (let i = 0; i < max; i++) {
      if (this.times[i] >= start || i + 1 >= max
          || this.times[i + 1] >= start) {
        return i;
      }
    }

    return max;
  }

  valueAt(when: bigint): string | number | undefined {
    const index = this.findUpdateBefore(when);

    if (index == this.times.length) {
      return undefined;
    }
    else {
      return (this.values[index] as string);
    }
  }

  reset() {
    this.times = [];
    this.values = [];
  }

  mapWaveform(start: bigint, end: bigint, fn: IWaveMap): void {
    const max = this.times.length;

    for (let i = this.findUpdateBefore(start); i < max; i++) {
      const when = this.times[i];
      const value = this.values[i] as string;
      const next = (i + 1 < max) ? this.times[i + 1] : end;

      if (when >= end)
        break;

      fn(when, next > end ? end : next, value, i);
    }
  }
}

export default Trace;
