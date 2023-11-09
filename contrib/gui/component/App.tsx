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

import type { Toaster } from "@blueprintjs/core";
import { FocusStyleManager, OverlayToaster } from "@blueprintjs/core";
import "@blueprintjs/core/lib/css/blueprint.css";
import React from "react";
import Split from "react-split";
import Conduit from "../lib/conduit";
import { Model } from "../lib/model";
import ClosedAlert from "./ClosedAlert";
import Transcript from "./Transcript";
import WaveView from "./WaveView";
import Sidebar from "./Sidebar";
import { bindEvents } from "../lib/react-util";

const socket = new WebSocket(`ws://${window.location.host}/socket`);
socket.binaryType = "arraybuffer";

const conduit = new Conduit(socket);
const model = new Model(conduit);
const toaster: Toaster = OverlayToaster.create({ position: "top-right" });

function showRestartToast() {
  toaster.show({ message: "simulation restarted" });
}

function setWindowTitle(top: string) {
  toaster.show({ message: `simulation ${top} loaded` });
  document.title = `${top.toLowerCase()} - NVC`;
}

function showQuitToast() {
  toaster.show({ message: "simulation unloaded" });
}

FocusStyleManager.onlyShowFocusOnTabs();

function App() {
  bindEvents(model, {
    restartSim: showRestartToast,
    quitSim: showQuitToast,
    startSim: setWindowTitle,
  });

  return (
    <div className="component-app">
      <Split className="app-split" direction="vertical" gutterSize={8}>
        <Split className="app-hsplit" gutterSize={8} sizes={[20, 80]}>
          <Sidebar />
          <WaveView model={model}/>
        </Split>
        <Transcript conduit={conduit} />
      </Split>
      <ClosedAlert conduit={conduit} />
    </div>
  );
}

export default App;
