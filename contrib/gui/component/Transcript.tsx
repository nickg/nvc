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

import React, { useEffect, useRef, useState } from "react";
import type Conduit from "../lib/conduit";
import Convert from "ansi-to-html";
import "./Transcript.css";

interface IProps {
  conduit: Conduit;
}

enum LogKind {
  INPUT,
  OUTPUT
}

type LogItem = {
  kind: LogKind;
  text: string;
};

const convert = new Convert();

function PromptChevron(props: { fill: string }) {
  return (
    <svg xmlns="http://www.w3.org/2000/svg"
         width="14" height="14" viewBox="0 0 16 16"
         fill="context-fill" fillOpacity="context-fill-opacity"
         style={{ fill: props.fill }}>
        <path d="M2.293 2.293a1 1 0 0 1 1.414 0l5 5a1 1 0 0
                 1 0 1.414l -5 5a1 1 0 0 1 -1.414 -1.414L6.586
                 8 2.293 3.707a1 1 0 0 1 0-1.414zM8.293 2.293a1
                 1 0 0 1 1.414 0l5 5a1 1 0 0 1 0 1.414l-5 5a1
                 1 0 0 1 -1.414 -1.414L12.586 8 8.293 3.707a1
                 1 0 0 1 0 -1.414z" />
    </svg>
  );
}

function Transcript(props: IProps) {
  const [command, setCommand] = useState("");
  const [log, setLog] = useState<Array<LogItem>>([]);
  const [history, setHistory] = useState<Array<string>>([]);
  const [historyPos, setHistoryPos] = useState(0);
  const [focus, setFocus] = useState(false);
  const historyRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLInputElement>(null);

  const appendInput = (text: string) => {
    const item: LogItem = {
      kind: LogKind.INPUT,
      text: text,
    };
    setLog((prev) => [...prev, item]);

    if (history.length == 0 || history[history.length - 1] != text) {
      setHistory([...history, text]);
      setHistoryPos(history.length + 1);
    }
  };

  const appendOutput = (text: string) => {
    const item: LogItem = {
      kind: LogKind.OUTPUT,
      text: convert.toHtml(text.replace(" ", "&nbsp")),
    };
    setLog((prev) => [...prev, item]);
  };

  const handleKeyUp = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === "Enter") {
      const trimmed = command.trim();
      if (trimmed.length > 0) {
        console.log(trimmed);
        props.conduit.evalTcl(trimmed);

        appendInput(command);
        setCommand("");
      }
    }
    else if (event.key === "ArrowUp") {
      if (historyPos > 0) {
        setCommand(history[historyPos - 1]);
        setHistoryPos(historyPos - 1);
      }
    }
    else if (event.key === "ArrowDown") {
      if (historyPos + 1 < history.length) {
        setCommand(history[historyPos + 1]);
        setHistoryPos(historyPos + 1);
      }
      else {
        setCommand("");
        setHistoryPos(history.length);
      }
    }
  };

  const handleKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === "Enter" || event.key == "ArrowUp"
        || event.key == "ArrowDown") {
      event.preventDefault();
    }
  };

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setCommand(event.target.value);
  };

  const renderLogItem = (item: LogItem, key: number) => {
    switch (item.kind) {
      case LogKind.INPUT:
        return (
          <div key={key} className="transcript-input-log"
               onClick={() => setCommand(item.text)}>
            <div className="transcript-icon">
              <PromptChevron fill={"#000000"} />
            </div>
            <div className="transcript-text">
              {item.text}
            </div>
          </div>
        );
      case LogKind.OUTPUT:
        return (
          <div key={key} className="transcript-output-log"
               dangerouslySetInnerHTML={{__html: item.text}} />
        );
    }
  };

  props.conduit.onConsoleOutput = (data) => {
    appendOutput(data);
  };

  props.conduit.onInitCommand = (cmd) => {
    appendInput(cmd);
    props.conduit.evalTcl(cmd);
  };

  useEffect(() => {
    // Scroll to the end of the history buffer when a new message is appended
    if (historyRef.current) {
      historyRef.current.scrollTop = historyRef.current.scrollHeight;
    }
  }, [log]);

  return (
    <div className="transcript">
      <div className="transcript-history" ref={historyRef}
           onClick={() => { inputRef.current?.focus(); }}>
        {log.map(renderLogItem)}
      </div>
      <div className="transcript-bottom">
        <div className="transcript-prompt">
          <PromptChevron fill={focus ? "#0066ff" : "#606060"} />
        </div>
        <input type="text" className="transcript-input"
               ref={inputRef}
               autoFocus spellCheck="false"
               onChange={handleChange}
               onKeyUp={handleKeyUp}
               onKeyDown={handleKeyDown}
               onFocus={() => setFocus(true)}
               onBlur={() => setFocus(false)}
               value={command} />
      </div>
    </div>
  );
}

export default Transcript;
