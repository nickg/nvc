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

import type Conduit from "./conduit";
import Trace from "./trace";

export class Signal {
  private model: Model;
  private _path: string;
  private _trace: Trace;

  constructor(model: Model, path: string, value: string) {
    this.model = model;
    this._path = path;
    this._trace = new Trace(value);
  }

  get path() {
    return this._path;
  }

  get trace() {
    return this._trace;
  }

  update(value: string) {
    this._trace.update(this.model.now, value);
  }

  reset() {
    this._trace.reset();
  }
}

type NextTimeStepFn = (now: bigint) => void;
type AddWaveFn = (s: Signal) => void;
type RestartFn = () => void;
type QuitFn = () => void;
type StartSimFn = (top: string) => void;

class EventObserver<T extends ((...args: any[]) => void)> {
  private observers: Array<T> = [];

  subscribe(func: T) {
    this.observers.push(func);
  }

  unsubscribe(func: T) {
    this.observers = this.observers.filter((observer) => observer !== func);
  }

  notify(...args: Parameters<T>) {
    for (const func of this.observers) {
      func(...args);
    }
  }
}

type EventMap = { [index: string]: EventObserver<any> };

export class Model {
  private _now: bigint;
  private signals: Map<string, Signal> = new Map();

  private _events: EventMap = {
    addWave: new EventObserver<AddWaveFn>(),
    nextTimeStep: new EventObserver<NextTimeStepFn>(),
    restartSim: new EventObserver<RestartFn>(),
    quitSim: new EventObserver<QuitFn>(),
    startSim: new EventObserver<StartSimFn>(),
  };

  constructor(conduit: Conduit) {
    this._now = 0n;

    conduit.onOpen = () => {
      conduit.evalTcl("source $nvc_dataDir/gui/guilib.tcl");
    };

    conduit.onBackchannel = (obj: any) => {
      console.log(obj);
    };

    conduit.onNextTimeStep = (now: bigint) => {
      this._now = now;
      this._events.nextTimeStep.notify(now);
    };

    conduit.onAddWave = (path: string, value: string) => {
      if (!this.signals.has(path)) {
        const s = new Signal(this, path, value);
        this.signals.set(path, s);
        this._events.addWave.notify(s);
      }
    };

    conduit.onSignalUpdate = (path: string, value: string) => {
      const s = this.signals.get(path);
      if (s === undefined) {
        console.warn("skipping update for unknown signal " + path);
      }
      else {
        s.update(value);
      }
    };

    conduit.onRestartSim = () => {
      this._now = 0n;
      this.signals.forEach(s => s.reset());

      this._events.restartSim.notify();
      this._events.nextTimeStep.notify(0n);
    };

    conduit.onQuitSim = () => {
      this._now = 0n;
      this.signals.clear();

      this._events.quitSim.notify();
    };

    conduit.onStartSim = (top: string) => {
      this._now = 0n;
      this.signals.clear();

      this._events.startSim.notify(top);
      this._events.nextTimeStep.notify(0n);

      conduit.evalTcl("::nvc::RefreshRegions");
    };
  }

  get now() {
    return this._now;
  }

  event(name: string): EventObserver<any> {
    if (name in this._events) {
      return this._events[name];
    }
    else {
      throw new Error(`invalid event ${name}`);
    }
  }

  signal(path: string): Signal {
    const s = this.signals.get(path);
    if (s === undefined) {
      throw new Error(`signal ${path} does not exist`);
    }
    else {
      return s;
    }
  }
}
