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
        points: [],
        graph : {
            root: null,
            last: null
        },
        vega: null,
        resizing: false,
        flameGraph: null
    }

    export function init() {
        // update the profile source info
        document.getElementById("name").innerHTML = "Timeline: " + Data.metadata.name;
        document.getElementById("source").innerHTML = Data.metadata.source;
        document.getElementById("form").innerHTML = Data.metadata.form;
        document.getElementById("time").innerHTML = Data.metadata.time;
        document.title = "Timeline: " + Data.metadata.name;

        // Prepare data for the timeline and flame graph
        initTimelineData();

        // Render the flame graph
        renderFlameGraph();

        // Render the timeline
        renderTimeline();

        window.addEventListener("resize", windowResizeCallback);
    }

    export function update() {
        //initTimelineData();
        //console.log(Timeline.points.length);
        //Timeline.vega.data("points").remove((d) => true);
        //Timeline.vega.data("points").insert(Timeline.points);
        //Timeline.vega.update();
    }

    function initTimelineData() {
        if (Data.events.length == 0) return;

        // compute delta from first event
        let first = Data.events[0]["metrics"];
        let computePoint = (p: Object) => {
            let ret = {};
            for (let k of Object.keys(p)) {
                if (first.hasOwnProperty(k))
                    ret[k] = p[k] - first[k];
            }
            return ret;
        }

        // build up three things by walking the list of events:
        //  * a list of points on the graph (in Timeline.points)
        //  * a list of breakpoints for scrubbing (Timeline.breaks)
        //  * the callgraph (in Timeline.graph)
        let breaks = [];
        let points = [];
        let graph = Timeline.graph.last;
        for (let e of Data.events) {
            if (e["type"] == "ENTER") {
                let p = computePoint(e["metrics"]);
                let node = {
                    "name": e["function"],
                    "start": p["time"],
                    "enter": p,
                    "parentPtr": graph,
                    "children": []
                };
                if (graph === null) {
                    Timeline.graph.root = node;
                    node.parentPtr = node;
                } else {
                    graph.children.push(node);
                }
                graph = node;
                points.push(p);
                breaks.push([p["time"], node, p]);
            }
            else if (e["type"] == "EXIT") {
                let p = computePoint(e["metrics"]);
                if (graph === null) {
                    console.error("unbalanced events: EXIT with null graph");
                    return;
                }
                graph["finish"] = p["time"];
                graph["value"] = p["time"] - graph["start"];
                graph["exit"] = p;
                points.push(p);
                breaks.push([p["time"], graph, p]);
                graph = graph.parentPtr;
            }
        }

        Timeline.graph.last = graph;
        Timeline.points = points;
        Timeline.breaks = breaks; // done last for race condition
    }


    function renderTimeline() {
        let timeline = document.getElementById("timeline");

        let keys = {};
        for (let p of Timeline.points) {
            for (let k in p) {
                keys[k] = true;
            }
        }
        if (keys.hasOwnProperty("time"))
            delete keys["time"];
        let keyNames = Object.keys(keys);

        let scales = keyNames.map((k) => {
            return {
                "name": k,
                "type": "linear",
                "domain": { "data": "points", "field": k },
                "range": "height",
                "round": true,
                "nice": true
            };
        });

        // render the vega spec
        let spec = {
            "width": 800,
            "height": 400,
            "padding": "auto",
            "data": [{
                "name": "points",
                "values": Timeline.points,
                "transform": [
                    { "type": "fold", "fields": keyNames },
                    { "type": "sort", "by": "-time" }
                ]
            }],
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
                "name": "color",
                "type": "ordinal",
                "domain": { "data": "points", "field": "key"},
                "range": "category10"
            }].concat(scales as any),
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
            "legends": [{
                "fill": "color",
                "orient": "left",
                "offset": -150,
                "properties": {
                    "labels": {"fontSize": {"value": 12}},
                    "symbols": {"stroke": {"value": "transparent"}}
                }
            }],
            "marks": [{
                "type": "group",
                "from": { 
                    "data": "points", 
                    "transform": [{
                        "type": "facet",
                        "groupby": ["key"]
                    }]
                },
                "marks": [{
                    "name": "y",
                    "type": "line",
                    "properties": {
                        "update": {
                            "x": { "scale": "x", "field": "time" },
                            "y": { "scale": {"datum": "key"}, "field": "value" },
                            "stroke": { "scale": "color", "field": "key" },
                            "strokeWidth": { "value": 2 }
                        }
                    }
                }]
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
                        "fillOpacity": {"signal": "xhoveropacity"}
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
        var i;
        for (i = 0; i < Timeline.breaks.length - 1; i++) {  // the last entry is the fallback for value > max(time)
            if (value < Timeline.breaks[i][0])
                break;
        }
        let [_, graphNode, metrics] = Timeline.breaks[i];

        // render values
        let keys = Object.keys(metrics);
        keys.sort();
        for (let k of keys) {
            let node = document.createElement("li");
            let val = (metrics[k] % 1 == 0) ? metrics[k] : metrics[k].toFixed(2);
            node.innerHTML = "<b>" + k + "</b>: " + val;
            values.insertAdjacentElement("beforeend", node);
        }

        // figure out the stack
        let stack = [];
        while (graphNode.parentPtr != graphNode) {
            let name = graphNode["name"];
            graphNode = graphNode.parentPtr;

            if (name == "#f") continue;
            if (stack.length > 0) {
                let last = stack[stack.length-1];
                let lastName = (last instanceof Array ? last[0] : last);
                if (lastName == name) {
                    if (!(last instanceof Array))
                        stack[stack.length-1] = [name, 1];
                    stack[stack.length-1][1]++;
                } else {
                    stack.push(name);
                }
            } else {
                stack.push(name);
            }
        }

        // render the stack
        for (let entry of stack) {
            let node = document.createElement("li");
            let code = document.createElement("span");
            code.classList.add("code");

            let name = (entry instanceof Array ? entry[0] : entry);
            code.innerHTML = name;
            node.insertAdjacentElement("beforeend", code);

            if (entry instanceof Array) {
                let count = document.createElement("span");
                count.classList.add("counter");
                count.innerHTML = entry[1];
                node.insertAdjacentElement("beforeend", count);
            }

            functions.insertAdjacentElement("beforeend", node);
        }

        // highlight flamegraph
        if (Timeline.flameGraph) {
            Timeline.flameGraph.highlight(value);
        }
    }

    function renderFlameGraph() {
        Timeline.flameGraph = d3.flameGraph("#flamegraph", Timeline.graph.root);
        Timeline.flameGraph.zoomAction(flamegraphZoomCallback);
        Timeline.flameGraph.hoverAction(flamegraphHoverCallback);
        Timeline.flameGraph.render();
    }

    function windowResizeCallback() {
        if (Timeline.vega && !Timeline.resizing) {
            Timeline.resizing = true;
            window.setTimeout(() => {
                let stackWidth = document.getElementById("stack").clientWidth;
                let windowWidth = window.innerWidth;
                let padding = Timeline.vega.padding();
                let pad = padding.left + padding.right;
                let newWidth = windowWidth - stackWidth - pad - 80;
                Timeline.vega.width(newWidth).update();
                if (Timeline.flameGraph) {
                    Timeline.flameGraph.size([newWidth, 400]).render();
                    document.getElementById("flamegraph").style.marginLeft = padding.left;
                }
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
        if (!Timeline.vega) return;
        let start = node["start"];
        let finish = node["finish"];
        if (enter) {
            Timeline.vega.signal("xminhover", node["start"]);
            Timeline.vega.signal("xmaxhover", node["finish"]);
            Timeline.vega.signal("xhoveropacity", 0.2);
            Timeline.vega.signal("xtime", node["start"] + 1e-10);
        } else {
            Timeline.vega.signal("xhoveropacity", 0.0);
        }
        Timeline.vega.update();
    }

} // /namespace

document.addEventListener("DOMContentLoaded", dataOnload(timeline.init, timeline.update));