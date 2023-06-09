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

import { LRUCache } from "lru-cache";

class MeasureText {
  private context: CanvasRenderingContext2D;
  private cache: LRUCache<string, number>;

  constructor(font: string) {
    const canvas = document.createElement("canvas");

    const context = canvas.getContext("2d");
    if (context === null)
      throw new Error("cannot get 2d context");

    this.context = context;
    this.context.font = font;

    this.cache = new LRUCache({ max: 500 });
  }

  getTextWidth(text: string) {
    const cached = this.cache.get(text);
    if (cached)
      return cached;
    else {
      const width = this.context.measureText(text).width;
      this.cache.set(text, width);
      return width;
    }
  }
}

export default MeasureText;
