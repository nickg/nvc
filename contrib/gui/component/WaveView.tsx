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

import React from "react";
import { ResizeEntry, ResizeSensor } from "@blueprintjs/core";
import Split from "react-split";
import { useState, useCallback, useRef } from "react";
import type { Model, Signal } from "../lib/model";
import { formatTime } from "../lib/time-util";
import { DataType } from "../lib/trace";
import MeasureText from "../lib/measure-text";
import Toolbar from "./Toolbar";
import "./WaveView.css";
import { bindEvents } from "../lib/react-util";

interface IWaveSVGProps {
  width: number | string;
  height: number | string;
  busSlope: number;
  onClick?: React.MouseEventHandler;
}

function WaveSVG(props: React.PropsWithChildren<IWaveSVGProps>) {
  return (
    <svg xmlns="http://www.w3.org/2000/svg"
         xmlnsXlink="http://www.w3.org/1999/xlink" version="1.1"
         onClick={props.onClick}
         width={props.width} height={props.height}>
      <defs>
        <g id="bus" shapeRendering="crispEdges">
          <line x1="0" y1="18.5" x2="1" y2="18.5" className="wave-bus-line" />
          <line x1="0" y1="2" x2="1" y2="2" className="wave-bus-line" />
        </g>
        <g id="busChange">
          <line x1="0" y1="18.5" x2={props.busSlope} y2="2"
                className="wave-bus-line wave-wide" />
          <line x1="0" y1="2" x2={props.busSlope} y2="18.5"
                className="wave-bus-line wave-wide" />
        </g>
        <g id="logic1" shapeRendering="crispEdges">
          <line x1="0" y1="2" x2="1" y2="2"
                className="wave-logic-1 wave-wide" />
        </g>
        <g id="logic0" shapeRendering="crispEdges">
          <line x1="0" y1="18" x2="1" y2="18" className="wave-logic-0" />
        </g>
        <g id="logicZ" shapeRendering="crispEdges">
          <line x1="0" y1="10" x2="1" y2="10" className="wave-logic-Z" />
        </g>
        <g id="logicX" shapeRendering="crispEdges">
          <rect x="0" y="10" width="1" height="9"
                className="wave-logic-X wave-box" />
          <line x1="0" y1="10" x2="1" y2="10" className="wave-logic-X" />
        </g>
        <g id="logic-" shapeRendering="crispEdges">
          <line x1="0" y1="10" x2="1" y2="10" className="wave-logic--" />
        </g>
        <g id="logicH" shapeRendering="crispEdges">
          <line x1="0" y1="2" x2="1" y2="2" className="wave-logic-H" />
        </g>
        <g id="logicL" shapeRendering="crispEdges">
          <line x1="0" y1="18" x2="1" y2="18" className="wave-logic-L" />
        </g>
        <g id="logicU" shapeRendering="crispEdges">
          <rect x="0" y="2" width="1" height="17"
                className="wave-logic-U wave-box" />
          <line x1="0" y1="2" x2="1" y2="2"
                className="wave-logic-U wave-wide" />
        </g>
        <g id="logicChange01" shapeRendering="crispEdges">
          <line x1="0" y1="18.5" x2="0" y2="2" className="wave-logic-1" />
        </g>
        <g id="logicChange0-" shapeRendering="crispEdges">
          <line x1="0" y1="18.5" x2="0" y2="10" className="wave-logic--" />
        </g>
        <g id="logicChangeU1" shapeRendering="crispEdges">
          <use xlinkHref="#logicChange01"/>
        </g>
        <g id="logicChangeUX" shapeRendering="crispEdges">
          <line x1="0" y1="1" x2="0" y2="10" className="wave-logic-X" />
        </g>
        <g id="logicChange10" shapeRendering="crispEdges">
          <line x1="0" y1="1" x2="0" y2="18" className="wave-logic-0" />
        </g>
        <g id="logicChange1-" shapeRendering="crispEdges">
          <line x1="0" y1="1" x2="0" y2="10" className="wave-logic--" />
        </g>
        <g id="logicChangeU0" shapeRendering="crispEdges">
          <use xlinkHref="#logicChange10"/>
        </g>
        <g id="logicChangeX0" shapeRendering="crispEdges">
          <line x1="0" y1="10" x2="0" y2="18" className="wave-logic-0" />
        </g>
        <g id="logicChangeX1" shapeRendering="crispEdges">
          <line x1="0" y1="10" x2="0" y2="2" className="wave-logic-1" />
        </g>
        <g id="logicChange-0" shapeRendering="crispEdges">
          <use xlinkHref="#logicChangeX0"/>
        </g>
        <g id="logicChange-1" shapeRendering="crispEdges">
          <use xlinkHref="#logicChangeX1"/>
        </g>
      </defs>
      {props.children}
    </svg>);
}

interface IProps {
  model: Model;
}

const measureText = new MeasureText("'Noto Sans', sans-serif; 12px");

function WaveView(props: IProps) {
  const [size, setSize] = useState([0, 0]);
  const [signals, setSignals] = useState<string[]>([]);
  const [now, setNow] = useState(0n);
  const [selSignal, setSelSignal] = useState(-1);
  const [scale, setScale] = useState(100000n);
  const [scrollTop, setScrollTop] = useState(0);
  const [scrollLeft, setScrollLeft] = useState(0);
  const [cursorPos, setCursorPos] = useState(-1n);

  const signalListRef = useRef<HTMLDivElement>(null);

  const rowHeight = 22;
  const toolbarHeight = 30;
  const busSlope = 5;
  const viewportHeight = size[1] - toolbarHeight;
  const viewportWidth = size[0];
  const visibleTime = BigInt(Math.ceil(viewportWidth)) * scale;
  const leftTime = BigInt(scrollLeft) * scale;
  const rightTime = leftTime + visibleTime;

  const handleResize = (entries: ResizeEntry[]) => {
    setSize([entries[0].contentRect.width, entries[0].contentRect.height]);
  };

  bindEvents(props.model, {
    nextTimeStep: (next: bigint) => { setNow(next); },
    addWave: (s: Signal) => { setSignals(prev => [...prev, s.path]); },
    quitSim: () => { setSignals([]); }
  });

  const handleScroll = useCallback((e: React.UIEvent<HTMLElement>) => {
    setScrollTop(e.currentTarget.scrollTop);
    setScrollLeft(e.currentTarget.scrollLeft);

    if (signalListRef.current)
      signalListRef.current.scrollTop = e.currentTarget.scrollTop;
  }, [scrollTop, scrollLeft]);

  const handleClick = (e: React.MouseEvent<HTMLElement>) => {
    const targetRect = e.currentTarget.getBoundingClientRect();
    const xPos = Math.round(e.pageX - targetRect.left);
    const clickTime = BigInt(xPos) * scale;
    console.log(clickTime);
    setCursorPos(clickTime);
  };

  //////////////////////////////////////////////////////////////////////
  // List of signals

  const signalList = signals.map((path, index) => {
    return (
      <div className="wave-list-name" key={index}
           onClick={() => setSelSignal(index)}
           style={{
             height: `${rowHeight}px`,
             background: index == selSignal ? "white" : "#00000000",
             color: index == selSignal ? "black" : "white"
           }} >
        {path}
      </div>);
  });

  //////////////////////////////////////////////////////////////////////
  // Waveforms

  const renderSignal = (path: string, index: number) => {
    const topY = index * rowHeight;
    const bottomY = (index + 1) * rowHeight;

    if (bottomY < scrollTop || topY > scrollTop + viewportHeight)
      return;   // Clipped

    const s = props.model.signal(path);
    const points: React.ReactElement[] = [];

    const start = BigInt(scrollLeft) * scale;
    const end = start + visibleTime > props.model.now
      ? props.model.now : start + visibleTime;
    const dataType = s.trace.dataType;
    const starWidth = measureText.getTextWidth("*");

    let last = s.trace.valueAt(start);

    s.trace.mapWaveform(start, end, (when, next, value, i) => {
      const leftClip = start - 600n * scale;
      const clipWhen = when < leftClip ? leftClip : when;
      const width = Math.ceil(Number((next - clipWhen)) / Number(scale));
      const xPos = clipWhen / scale;

      if (dataType == DataType.BIT || dataType == DataType.STD_LOGIC) {
        points.push(
          <g transform={`translate(${xPos},0)`} key={i}>
            <use xlinkHref={`#logicChange${last}${value}`}/>
            <use xlinkHref={`#logic${value}`} transform={`scale(${width}, 1)`}/>
          </g>);
      }
      else {
        const busTransform =
          `translate(${busSlope}, 0) scale(${width - busSlope}, 1)`;
        const textXOffset = busSlope + 2;
        const textBoxWidth = width - textXOffset - 2;
        const textWidth = measureText.getTextWidth(value);

        if (textBoxWidth >= starWidth) {
          points.push(
            <g transform={`translate(${xPos},0)`} key={i}>
              <use xlinkHref="#busChange"/>
              <use xlinkHref="#bus" transform={busTransform}/>
              <title>{value}</title>
              <rect x={textXOffset} y="3" width={textBoxWidth} height="14"/>
              <text x={textXOffset} y="14" className="wave-value-text">
                {textBoxWidth >= textWidth ? value : "*"}
              </text>
            </g>);
        }
        else {
          points.push(
            <g transform={`translate(${xPos},0)`} key={i}>
              <use xlinkHref="#busChange"/>
              <use xlinkHref="#bus" transform={busTransform}/>
            </g>);
        }
      }

      last = value;
    });

    return (
      <g key={index} transform={`translate(0,${index*rowHeight})`}>
        <g transform={"translate(0,0)"}>
          {points}
        </g>
      </g>
    );
  };

  //////////////////////////////////////////////////////////////////////
  // Cursors

  const renderCursor = () => {
    if (cursorPos < leftTime || cursorPos > rightTime)
      return;

    const xPos = Number(cursorPos / scale);

    return (
      <line x1={xPos} x2={xPos} y1={0} y2={signals.length * rowHeight}
            className="wave-cursor wave-cursor-1" shapeRendering="crispEdges"/>
    );
  };

  //////////////////////////////////////////////////////////////////////
  // Ruler

  const renderRuler = () => {
    const pixelsPerMark = 60;
    const numMarks = Math.max(1, Math.floor(viewportWidth / pixelsPerMark));
    const visibleTime = viewportWidth * Number(scale);
    const rawSpacing = visibleTime / numMarks;
    const spacing = Math.pow(10, Math.ceil(Math.log10(rawSpacing)));
    const actualMarks = visibleTime / spacing;
    const markPixels = spacing / Number(scale);

    const leftSlop = spacing > 0 ? Number(leftTime % BigInt(spacing)) : 0;

    let xShift = 0, leftTimeAdj = leftTime;
    if (leftSlop > 0) {
      xShift = (spacing - leftSlop) / Number(scale);
      leftTimeAdj += BigInt(spacing - leftSlop);
    }

    const marks = [];
    for (let i = -1; i < actualMarks; i++) {
      const markTime = leftTimeAdj + BigInt(i * spacing);
      if (markTime > 0) {
        const xPos = i * markPixels + xShift;
        marks.push(<line x1={xPos} x2={xPos} y1="12" y2="20"
                         className="wave-ruler-line" key={marks.length}/>);
        marks.push(<text x={xPos} y="10" key={marks.length}
                         className="wave-ruler-text">
                     {formatTime(markTime)}
                   </text>);
      }
    }

    if (cursorPos >= leftTime && cursorPos <= rightTime) {
      const xPos = Number((cursorPos - leftTime) / scale);

      marks.push(
        <line x1={xPos} x2={xPos} y1="12" y2="20"
              className="wave-cursor wave-cursor-1"/>);
      marks.push(<text x={xPos} y="10" key={marks.length}
                       className="wave-ruler-text wave-cursor-1-text">
                   {formatTime(cursorPos)}
                 </text>);
    }

    return (
      <svg xmlns="http://www.w3.org/2000/svg"
           xmlnsXlink="http://www.w3.org/1999/xlink" version="1.1"
           width="100%" height={rowHeight}>
        <g shapeRendering="crispEdges">
          {marks}
        </g>
      </svg>);
  };

  //////////////////////////////////////////////////////////////////////
  // Container

  return (
    <ResizeSensor onResize={handleResize} >
      <div>
        <Toolbar now={now} height={toolbarHeight}
                 onZoomIn={() => setScale(scale / 2n)}
                 onZoomOut={() => setScale(scale * 2n)} />
        <div className="wave-container"
             style={{height: viewportHeight}}>
          <Split className="wave-hsplit" gutterSize={4} sizes={[20, 80]}>
            <div style={{height: viewportHeight,
                         overflowY: "hidden",
                         overflowX: "auto"}}
                 className="wave-signals-list" ref={signalListRef}>
              <div style={{height: rowHeight}} className="wave-ruler-left"/>
              {signalList}
            </div>
            <div style={{height: viewportHeight,
                         overflowY: "auto",
                         overflowX: "scroll"}}
                 onScroll={handleScroll}>
              <div style={{height: rowHeight}} className="wave-ruler-right">
                {renderRuler()}
              </div>
              <WaveSVG width={Number(props.model.now / scale)}
                       height={signals.length * rowHeight}
                       onClick={handleClick}
                       busSlope={busSlope}>
                {signals.map((s, i) => renderSignal(s, i))}
                {renderCursor()}
              </WaveSVG>
            </div>
          </Split>
        </div>
      </div>
    </ResizeSensor>
  );
}

export default WaveView;
