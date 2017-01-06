/// <reference path="data.ts" />

// convince TS that document.querySelectorAll can be an array
interface NodeList extends Array<Node> { }
interface HTMLCollection extends Array<Element> { }

namespace timeline {

    declare var d3; // d3
    declare var vg; // vega
    declare var vl; // vega-lite

    export let Timeline = {
        breaks: [],
        stacks: [],
        points: [],
        vega: null,
        resizing: false,
        flameGraph: null
    }

    export function init() {
        // Initialize the profile data
        // initData();
        // Cache the stacks at each timestep
        initTimelineData();

        // update the profile source info
        document.getElementById("name").innerHTML = "Timeline: " + Data.metadata.name;
        document.getElementById("source").innerHTML = Data.metadata.source;
        document.getElementById("form").innerHTML = Data.metadata.form;
        document.getElementById("time").innerHTML = Data.metadata.time;
        document.title = "Timeline: " + Data.metadata.name;

        // Render the flame graph
        renderFlameGraph();

        // Render the timeline
        renderTimeline();

        

        // Bind the resize handler
        // window.addEventListener("resize", windowResizeCallback);
    }

    export function update() {
        let oldLength = Timeline.points.length;
        initTimelineData();
        if (Timeline.points.length > oldLength) {
            let newPoints = Timeline.points.slice(oldLength);
            Timeline.vega.data("points").insert(newPoints);
            Timeline.vega.update();
        }
    }

    function initTimelineData() {
        // walk the profile graph
        let root = Data.graph;
        let first = root["start"];
        // compute difference between a point and the first point
        let computePoint = (p: Object) => {
            let ret = {};
            for (let k of Object.keys(p)) {
                if (first.hasOwnProperty(k))
                    ret[k] = p[k] - first[k];
            }
            return ret;
        }
        let stack = [];
        let breaks = [], stacks = [], points = [];
        let rec = (node) => {
            let start = computePoint(node["start"]);
            // push start data point
            stack.push(node);
            let children = node["children"].slice() as Array<any>;
            children.sort((a, b) => a["start"]["time"] - b["start"]["time"]);
            for (let c of children) {
                // before entry, current stack
                let p = computePoint(c["start"]);
                breaks.push(p["time"]);
                stacks.push(stack.slice());
                points.push(p);
                // recurse on c, will push a pre-exit stack
                rec(c);
            }
            let finish = computePoint(node["finish"]);
            // push pre-exit stack
            breaks.push(finish["time"]);
            stacks.push(stack.slice());
            points.push(finish);
            // remove self from stack
            stack.pop();
        };
        rec(root);
        points = points.concat(Data.samples.map(computePoint));
        points.sort((a,b) => a["time"] - b["time"]);
        Timeline.points = points;
        Timeline.stacks = stacks;
        Timeline.breaks = breaks; // done last for race condition
    }

    function renderTimeline() {
        let timeline = document.getElementById("timeline");

        // render the vega spec
        let spec = {
            "width": 800,
            "height": 400,
            "padding": "auto",
            "data": [{ "name": "points", "values": Timeline.points }],
            "scales": [{
                "name": "x",
                "type": "linear",
                "domain": { "data": "points", "field": "time" },
                "domainMin": {"signal": "xmin"},
                "domainMax": {"signal": "xmax"},
                "range": "width",
                "round": true,
                "zero": false
            },
            {
                "name": "term-count",
                "type": "linear",
                "domain": { "data": "points", "field": "term-count" },
                "range": "height",
                "round": true,
                "nice": true
            },
            {
                "name": "merge-count",
                "type": "linear",
                "domain": { "data": "points", "field": "merge-count" },
                "range": "height",
                "round": true,
                "nice": true
            }],
            "signals": [{
                "name": "xtime",
                "init": 0,
                "streams": [{
                    "type": "mousemove",
                    "expr": "clamp(eventX(), 0, eventGroup('root').width)",
                    "scale": { "name": "x", "invert": true }
                }]
            }, 
            {"name": "xmin"},
            {"name": "xmax"},
            {"name": "xminhover"},
            {"name": "xmaxhover"},
            {"name": "xhoveropacity", "init": 0}
            ],
            "axes": [{
                "type": "x",
                "scale": "x",
                "format": "s",
                "grid": true,
                "layer": "back",
                "ticks": 5,
                "title": "time"
            },
            {
                "type": "y",
                "scale": "term-count",
                "format": "s",
                "grid": true,
                "layer": "back",
                "title": "term-count"
            },
            {
                "type": "y",
                "scale": "merge-count",
                "format": "s",
                "grid": false,
                "layer": "back",
                "title": "merge-count",
                "orient": "right"
            }],
            "marks": [{
                "name": "term-count",
                "type": "line",
                "from": { "data": "points", "transform": [{ "type": "sort", "by": "-time" }] },
                "properties": {
                    "update": {
                        "x": { "scale": "x", "field": "time" },
                        "y": { "scale": "term-count", "field": "term-count" },
                        "stroke": { "value": "#4682b4" },
                        "strokeWidth": { "value": 2 }
                    }
                }
            },
            {
                "name": "merge-count",
                "type": "line",
                "from": { "data": "points", "transform": [{ "type": "sort", "by": "-time" }] },
                "properties": {
                    "update": {
                        "x": { "scale": "x", "field": "time" },
                        "y": { "scale": "merge-count", "field": "merge-count" },
                        "stroke": { "value": "#01BA38" },
                        "strokeWidth": { "value": 2 }
                    }
                }
            },
            {
                "type": "rule",
                "properties": {
                    "update": {
                        "x": { "scale": "x", "signal": "xtime" },
                        "y": { "value": 0 },
                        "y2": { "field": { "group": "height" } },
                        "stroke": { "value": "red" }
                    }
                }
            },
            {
                "type": "rect",
                "properties": {
                    "update": {
                        "x": {"scale": "x", "signal": "xminhover"},
                        "x2": {"scale": "x", "signal": "xmaxhover"},
                        "y": {"value": 0},
                        "y2": {"field": {"group": "height"}},
                        "fill": {"value": "grey"},
                        "fillOpacity": {"signal": "xhoveropacity"}//{"value": 0.5}
                    }
                }
            }]
        }

        vg.embed(timeline, { spec: spec }, function (err, res) {
            if (err) {
                console.error(err);
            } else {
                Timeline.vega = res.view;
                res.view.onSignal('xtime', timelineScrubCallback);
                // resize the graph
                windowResizeCallback();
            }
        });

        // initialize the callstack
        timelineScrubCallback("xtime", 0);
    }

    function timelineScrubCallback(signal: string, value: number) {
        let values = document.getElementById("stack-values");
        let functions = document.getElementById("stack-functions");
        // remove existing entries
        while (values.firstChild)
            values.removeChild(values.firstChild);
        while (functions.firstChild)
            functions.removeChild(functions.firstChild);
        
        // find the appropriate stack
        var i = 0;
        for (i = 0; i < Timeline.breaks.length; i++) {
            if (value <= Timeline.breaks[i])
                break;
        }
        let stack = Timeline.stacks[i];
        var j = 0;
        for (j = 0; j < Timeline.points.length; j++) {
            if (value <= Timeline.points[j]["time"])
                break;
        }
        let point = Timeline.points[j];

        // render values
        let keys = Object.keys(point);
        keys.sort();
        for (let k of keys) {
            let node = document.createElement("li");
            let val = (point[k] % 1 == 0) ? point[k] : point[k].toFixed(2);
            node.innerHTML = "<b>" + k + "</b>: " + val;
            values.insertAdjacentElement("beforeend", node);
        }

        // render new stack
        let lastEntry = null;
        let lastNode = null as HTMLLIElement;
        for (let entry of stack) {
            if (lastEntry && entry["function"] == lastEntry["function"]) {
                var counter = lastNode.querySelector(".counter");
                if (!counter) {
                    counter = document.createElement("span");
                    counter.innerHTML = "0";
                    counter.classList.add("counter");
                    lastNode.insertAdjacentElement("beforeend", counter);
                }
                counter.innerHTML = (parseInt(counter.innerHTML) + 1).toString();
            } else {
                if (entry["function"] == "#f")
                    continue;
                let node = document.createElement("li");
                let code = document.createElement("span");
                code.innerHTML = getFunctionName(entry["function"]);
                code.classList.add("code");
                node.insertAdjacentElement("beforeend", code);
                functions.insertAdjacentElement("beforeend", node);
                lastNode = node;
                lastEntry = entry;
            }
        }
    }

    function renderFlameGraph() {
        let dt = Data.graph["start"]["time"];
        let rec = (node) => {
            let children = node["children"].map(rec);
            children.sort((a, b) => a["start"] - b["start"]);
            return {
                "name": node["function"],
                "value": node["finish"]["time"] - node["start"]["time"],
                "start": node["start"]["time"] - dt,
                "finish": node["finish"]["time"] - dt,
                "children": children
            };
        };
        let graph = rec(Data.graph);
        Timeline.flameGraph = d3.flameGraph("#flamegraph", graph);
        Timeline.flameGraph.zoomAction(flamegraphZoomCallback);
        Timeline.flameGraph.hoverAction(flamegraphHoverCallback);
        Timeline.flameGraph.render();
    }

    function windowResizeCallback() {
        if (Timeline.vega && !Timeline.resizing) {
            Timeline.resizing = true;
            window.setTimeout(() => {
                let panel = document.getElementById("timeline-panel");
                let width = panel.clientWidth;
                Timeline.vega.width(width-150).update();
                Timeline.flameGraph.size([width-150, 400]).render();
                document.getElementById("flamegraph").style.marginLeft = Timeline.vega.padding().left;
                Timeline.resizing = false;
            }, 50);
        }
    }

    function flamegraphZoomCallback(node) {
        Timeline.vega.signal("xmin", node["start"]);
        Timeline.vega.signal("xmax", node["finish"]);
        Timeline.vega.update();
    }

    function flamegraphHoverCallback(node, enter) {
        let start = node["start"];
        let finish = node["finish"];
        if (enter) {
            Timeline.vega.signal("xminhover", node["start"]);
            Timeline.vega.signal("xmaxhover", node["finish"]);
            Timeline.vega.signal("xhoveropacity", 0.2);
            Timeline.vega.signal("xtime", node["start"] + 1e-10);
            Timeline.vega.update();
        } else {
            Timeline.vega.signal("xhoveropacity", 0.0);
            Timeline.vega.update();
        }
    }

} // /namespace

document.addEventListener("DOMContentLoaded", dataOnload(timeline.init, timeline.update));