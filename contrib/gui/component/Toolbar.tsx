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

import { Navbar, Button, Alignment, Classes } from "@blueprintjs/core";
import React from "react";
import { formatTime } from "../lib/time-util";

interface IProps {
  now: bigint;
  height: number;
  onZoomIn?: () => void;
  onZoomOut?: () => void;
}

function Toolbar({now, height, onZoomIn, onZoomOut}: IProps) {
  return (
    <Navbar fixedToTop={false} style={{height: `${height}px`}}>
      <Navbar.Group align={Alignment.LEFT} style={{height: `${height}px`}}>
        <Navbar.Heading>{formatTime(now)}</Navbar.Heading>
        <Navbar.Divider />
        <Button className={Classes.MINIMAL} icon="home" text="Home" />
        <Button className={Classes.MINIMAL} icon="document" text="Files" />
        <Navbar.Divider />
        <Button className={Classes.MINIMAL} icon="zoom-in"
                onClick={onZoomIn ? () => { onZoomIn(); } : undefined} />
        <Button className={Classes.MINIMAL} icon="zoom-out"
                onClick={onZoomOut ? () => { onZoomOut(); } : undefined} />
        <Button className={Classes.MINIMAL} icon="zoom-to-fit" />
      </Navbar.Group>
    </Navbar>
  );
}

export default Toolbar;
