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
        highlightRegions: [],
        firstEvent: null,
        graph : {
            root: null,
            last: null
        },
        vega: null,
        resizing: false,
        flameGraph: null
    }

    // ipts stands for infeasible pc times
    export function init(events, ipts) {
        // update the profile source info
        document.getElementById("name").innerHTML = "Timeline: " + escapeHtml(Data.metadata.name);
        document.getElementById("source").innerHTML = escapeHtml(Data.metadata.source);
        document.getElementById("form").innerHTML = escapeHtml(Data.metadata.form);
        document.getElementById("time").innerHTML = escapeHtml(Data.metadata.time);
        document.title = "Timeline: " + Data.metadata.name;

        // Prepare data for the timeline and flame graph
        computeTimelineData(events);
        computeHighlightRegions(ipts);

        // Render the flame graph
        renderFlameGraph();

        // Render the timeline
        renderTimeline();

        window.addEventListener("resize", windowResizeCallback);
    }

    // ipts stands for infeasible pc times
    export function update(events, ipts) {
        let oldPoints = Timeline.points.length;
        let oldRegions = Timeline.highlightRegions.length;
        computeTimelineData(events);
        computeHighlightRegions(ipts);
        if (Timeline.points.length - oldPoints > 0) {
            let newPoints = Timeline.points.slice(oldPoints);
            Timeline.vega.data("points").insert(newPoints);
            let first = Timeline.points[0]["time"];
            let last  = Timeline.points[Timeline.points.length - 1]["time"];
            let duration = last == first ? 1 : last - first;
            let width = Timeline.vega.width();
            let dt = duration / width / 2;  // fudge factor
            var mostRecent = -dt;
            Timeline.vega.data("points").remove((p) => {
                if (p["time"] >= mostRecent + dt) {
                    mostRecent = p["time"];
                    return false;
                }
                return true;
            });
        }
        if (Timeline.highlightRegions.length - oldRegions > 0) {
            let newRegions = Timeline.highlightRegions.slice(oldRegions);
            Timeline.vega.data("highlight").insert(newRegions);
        }
        Timeline.vega.update();
        renderFlameGraph();
    }

    function computeTimelineData(events) {
        if (events.length == 0) return;

        // compute delta from first event
        if (Timeline.firstEvent === null)
            Timeline.firstEvent = events[0]["metrics"];
        let computePoint = (p: Object) => {
            let ret = {};
            for (let k of Object.keys(p)) {
                if (Timeline.firstEvent.hasOwnProperty(k))
                    ret[k] = p[k] - Timeline.firstEvent[k];
            }
            return ret;
        }
        let computeDelta = (p1: Object, p2: Object) => {
            let ret = {};
            for (let k in p1) {
                if (p2.hasOwnProperty(k)) {
                    ret[k] = p2[k] - p1[k];
                }
            }
            return ret;
        }
        let computeExcl = (children: Array<Object>, incl: Object) => {
            let ret = {};
            for (let k in incl) {
                ret[k] = incl[k]
            };
            for (let c of children) {
                for (let k in incl) {
                    ret[k] -= c["incl"].hasOwnProperty(k) ? c["incl"][k] : 0;
                }
            }
            return ret;
        }

        // build up three things by walking the list of events:
        //  * a list of points on the graph (in Timeline.points)
        //  * a list of breakpoints for scrubbing (Timeline.breaks)
        //  * the callgraph (in Timeline.graph)
        let breaks = [];
        let points = [];
        if (Timeline.graph.root == null) {
            Timeline.graph.root = {
                "name": "root",
                "start": 0,
                "children": []
            };
            Timeline.graph.last = Timeline.graph.root;
        }
        let graph = Timeline.graph.last;
        for (let e of events) {
            if (e["type"] == "ENTER") {
                let p = computePoint(e["metrics"]);
                let node = {
                    "name": e["function"],
                    "start": p["time"],
                    "enter": p,
                    "parentPtr": graph,
                    "children": []
                };
                graph.children.push(node);
                graph = node;
                points.push(p);
                breaks.push([p["time"], node, p]);
            } else if (e["type"] == "EXIT") {
                let p = computePoint(e["metrics"]);
                if (graph === Timeline.graph.root) {
                    console.error("unbalanced events: EXIT with null graph");
                    return;
                }
                graph["finish"] = p["time"];
                graph["value"] = p["time"] - graph["start"];
                graph["exit"] = p;
                graph["incl"] = computeDelta(graph["enter"], graph["exit"]);
                graph["excl"] = computeExcl(graph["children"], graph["incl"]);
                points.push(p);
                breaks.push([p["time"], graph, p]);
                graph = graph.parentPtr;
            } else if (e["type"] == "SAMPLE") {
                let p = computePoint(e["metrics"]);
                points.push(p);
                breaks.push([p["time"], graph, p]);
            }
        }

        // insert fake finish times into un-closed graph nodes
        let finish = events[events.length-1]["metrics"]["time"] - Timeline.firstEvent["time"];
        let curr = graph;
        while (curr) {
            curr["finish"] = finish;
            curr["value"] = finish - curr["start"];
            curr = curr.parentPtr;
        }

        Timeline.graph.last = graph;
        for (let p of points) Timeline.points.push(p);
        for (let b of breaks) Timeline.breaks.push(b);
    }

    // ipts stands for infeasible pc times
    function computeHighlightRegions(ipts) {
        if (ipts.length == 0) return;

        let highlightRegions = [];
        for (let ipt of ipts) {
            highlightRegions.push({
                "time": ipt.start - Timeline.firstEvent["time"],
                "value": 1, // 1 means "start highlight"
                "type": "infeasible pc" // category of highlight
            });
            highlightRegions.push({
                "time": ipt.end - Timeline.firstEvent["time"],
                "value": 0, // 0 means "end highlight"
                "type": "infeasible pc" // category of highlight
            });
        }

        for (let h of highlightRegions) Timeline.highlightRegions.push(h);
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
            },
            {
                "name": "highlight",
                "values": Timeline.highlightRegions
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
            },
            {
                "name": "highlight",
                "type": "linear",
                "domain": [0, 1],
                "range": "height",
                "round": true
            },
            {
                "name": "highlight-color",
                "type": "ordinal",
                "domain": { "data": "highlight", "field": "type" },
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
                "title": "Metrics",
                "fill": "color",
                "orient": "right",
                "offset": 0,
                "properties": {
                    "labels": {"fontSize": {"value": 12}},
                    "symbols": {"stroke": {"value": "transparent"}}
                }
            },
            {
                "title": "Highlights",
                "fill": "highlight-color",
                "orient": "right",
                "offset": 0,
                "properties": {
                    "labels": {"fontSize": {"value": 12}},
                    "symbols": {"stroke": {"value": "transparent"}, "fillOpacity": {"value": 0.3}}
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
                "type": "group",
                "from": {
                    "data": "highlight",
                    "transform": [{
                        "type": "facet",
                        "groupby": ["type"]
                    }]
                },
                "marks": [{
                    "name": "highlight",
                    "type": "area",
                    "properties": {
                        "update": {
                            "x": { "scale": "x", "field": "time" },
                            "y": { "scale": "highlight", "field": "value" },
                            "y2": { "scale": "highlight", "value": 0 },
                            "interpolate": { "value": "step-after" },
                            "fill": { "scale": "highlight-color", "field": "type" },
                            "fillOpacity": { "value": 0.3 }
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
        while (graphNode) {
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
        let rootStart = Timeline.graph.root["start"];
        let rootFinish = Timeline.graph.root["finish"];
        let rootTime = rootFinish - rootStart;
        let width = Timeline.flameGraph ? Timeline.flameGraph.size()[0] : 1200;
        let minTime = rootTime / width;
        let rec = (node) => {
            let children = node["children"].filter((n) => n["finish"]-n["start"] > minTime).map(rec);
            let dt = node.hasOwnProperty("excl") ? node["excl"]["time"] : node["finish"] - node["start"];
            let val = Math.pow(dt / rootTime, 0.25);
            let r = 200 - Math.round(100 * val);
            let g = 200 - Math.round(100 * val)
            let b = 232;
            let color = `rgb(${r}, ${g}, ${b})`;
            return {
                "name": node["name"],
                "start": node["start"],
                "finish": node["finish"],
                "value": node["value"],
                "color": color,
                "children": children
            };
        };
        let root = rec(Timeline.graph.root);

        if (Timeline.flameGraph) {
            Timeline.flameGraph.data(root);
        } else {
            Timeline.flameGraph = d3.flameGraph("#flamegraph", root);
            Timeline.flameGraph.size([800, 300]);
            Timeline.flameGraph.zoomAction(flamegraphZoomCallback);
            Timeline.flameGraph.hoverAction(flamegraphHoverCallback);
        }
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
                    Timeline.flameGraph.size([newWidth, 300]).render();
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
