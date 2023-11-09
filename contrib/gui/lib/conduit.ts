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

enum ServerOpcode {
  S2C_ADD_WAVE = 0x00,
  S2C_SIGNAL_UPDATE = 0x01,
  S2C_INIT_CMD = 0x02,
  S2C_START_SIM = 0x03,
  S2C_RESTART_SIM = 0x04,
  S2C_QUIT_SIM = 0x05,
  S2C_NEXT_TIME_STEP = 0x06,
}

class PacketBuffer {
  private buffer: ArrayBuffer;
  private data: DataView;
  private pos: number = 0;

  constructor(buffer: ArrayBuffer) {
    this.buffer = buffer;
    this.data = new DataView(buffer);
  }

  public unpackU8(): number {
    return this.data.getUint8(this.pos++);
  }

  public unpackU32(): number {
    const value = this.data.getUint32(this.pos);
    this.pos += 4;
    return value;
  }

  public unpackU64(): bigint {
    const high = this.data.getUint32(this.pos);
    const low = this.data.getUint32(this.pos + 4);
    this.pos += 8;
    return (BigInt(high) << 32n) | BigInt(low);
  }

  public unpackString(): string {
    const len = this.data.getInt16(this.pos);
    const endpos = this.pos + 2 + len;
    const decoder = new TextDecoder();
    const str = decoder.decode(this.buffer.slice(this.pos + 2, endpos));
    this.pos = endpos;
    return str;
  }

  public unpackRaw(len: number): ArrayBuffer {
    const result = this.buffer.slice(this.pos, this.pos + len);
    this.pos += len;
    return result;
  }
}

interface IWebSocket {
  send(text: string): void;
  close(): void;
  onmessage: ((ev: MessageEvent) => any) | null;
  onclose: ((ev: CloseEvent) => any) | null;
  onopen: ((ev: Event) => any) | null;
}

class Conduit {
  private socket: IWebSocket;

  onConsoleOutput: (data: string) => void = console.log;
  onAddWave: (path: string, value: string) => void = () => {};
  onSignalUpdate: (path: string, value: string) => void = () => {};
  onInitCommand: (cmd: string) => void = console.log;
  onOpen: (() => void) | null = null;
  onClose: (() => void) | null = null;
  onStartSim: ((top: string) => void) | null = null;
  onRestartSim: (() => void) | null = null;
  onQuitSim: (() => void) | null = null;
  onNextTimeStep: (now: bigint) => void = () => {};

  constructor(socket: IWebSocket) {
    this.socket = socket;

    this.socket.onmessage = (e) => {
      if (typeof e.data == "string")
        this.onConsoleOutput(e.data);
      else
        this.parseMessage(e.data);
    };

    this.socket.onclose = () => {
      this.onClose?.();
    };

    this.socket.onopen = () => {
      this.onOpen?.();
    };
  }

  private parseMessage(buffer: ArrayBuffer) {
    const packet = new PacketBuffer(buffer);
    const op = packet.unpackU8() as ServerOpcode;

    switch (op) {
      case ServerOpcode.S2C_ADD_WAVE:
        this.parseAddWave(packet);
        break;
      case ServerOpcode.S2C_SIGNAL_UPDATE:
        this.parseSignalUpdate(packet);
        break;
      case ServerOpcode.S2C_INIT_CMD:
        this.parseInitCommand(packet);
        break;
      case ServerOpcode.S2C_START_SIM:
        this.parseStartSim(packet);
        break;
      case ServerOpcode.S2C_RESTART_SIM:
        this.onRestartSim?.();
        break;
      case ServerOpcode.S2C_QUIT_SIM:
        this.onQuitSim?.();
        break;
      case ServerOpcode.S2C_NEXT_TIME_STEP:
        this.parseNextTimeStep(packet);
        break;
      default:
        console.log("unhandled message " + op);
        break;
    }
  }

  private parseAddWave(packet: PacketBuffer) {
    const path = packet.unpackString();
    const value = packet.unpackString();
    this.onAddWave(path, value);
  }

  private parseNextTimeStep(packet: PacketBuffer) {
    this.onNextTimeStep(packet.unpackU64());
  }

  private parseInitCommand(packet: PacketBuffer) {
    this.onInitCommand(packet.unpackString());
  }

  private parseStartSim(packet: PacketBuffer) {
    this.onStartSim?.(packet.unpackString());
  }

  private parseSignalUpdate(packet: PacketBuffer) {
    const path = packet.unpackString();
    const value = packet.unpackString();
    this.onSignalUpdate(path, value);
  }

  public evalTcl(script: string) {
    this.socket.send(script);
  }

  public close() {
    this.socket.close();
  }
}

export default Conduit;
