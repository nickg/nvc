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

import { Alert, Intent } from "@blueprintjs/core";
import React, { useEffect, useState } from "react";
import type Conduit from "../lib/conduit";

interface IProps {
  conduit: Conduit;
}

function ClosedAlert(props: IProps) {
  const [closed, setClosed] = useState(false);

  useEffect(() => {
    props.conduit.onClose = () => { setClosed(true); };
  }, []);

  const onConfirm = () => { location.reload(); };

  return (
    <Alert confirmButtonText="Refresh"
           onConfirm={onConfirm}
           isOpen={closed}
           intent={Intent.DANGER}
           loading={false}>
      <p>Lost connection to server</p>
    </Alert>
  );
}

export default ClosedAlert;
