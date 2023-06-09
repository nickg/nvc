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

import Conduit from "../lib/conduit";
import WebSocket from "ws";
import { expect, test } from "@jest/globals";

class BrowserWebSocket {
  private socket: WebSocket;

  onmessage: ((ev: { data: string | ArrayBuffer }) => any) | null = null;
  onclose: ((ev: {[key: string]: any}) => any) | null = null;
  onopen: ((ev: object) => any) | null = null;

  constructor(url: string) {
    this.socket = new WebSocket(url);
    this.socket.binaryType = "arraybuffer";

    this.socket.on("open", () => { this.onopen?.({}); });

    this.socket.on("close", () => { this.onclose?.({}); });

    this.socket.on("error", console.error);

    this.socket.on("message", (data, isBinary) => {
      if (isBinary) {
        this.onmessage?.({ data: data as ArrayBuffer });
      }
      else {
        this.onmessage?.({ data: data.toString() });
      }
    });
  }

  send(text: string) {
    this.socket.send(text);
  }

  close() {
    this.socket.close();
  }
}

test("sanity", (done) => {
  const c = new Conduit((url) => new BrowserWebSocket(url));

  c.onOpen = () => {};

  c.onConsoleOutput = () => {};

  c.onStartSim = (top) => {
    expect(top).toBe("WORK.LFSR.elab");
    c.close();
  };

  c.onClose = done;
});
