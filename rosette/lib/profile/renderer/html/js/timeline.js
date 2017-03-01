/// <reference path="data.ts" />
var timeline;
(function (timeline_1) {
    timeline_1.Timeline = {
        breaks: [],
        points: [],
        highlightRegions: [],
        firstEvent: null,
        graph: {
            root: null,
            last: null
        },
        vega: null,
        resizing: false,
        flameGraph: null
    };
    // ipts stands for infeasible pc times
    function init(events, ipts) {
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
    timeline_1.init = init;
    // ipts stands for infeasible pc times
    function update(events, ipts) {
        var oldPoints = timeline_1.Timeline.points.length;
        var oldRegions = timeline_1.Timeline.highlightRegions.length;
        computeTimelineData(events);
        computeHighlightRegions(ipts);
        if (timeline_1.Timeline.points.length - oldPoints > 0) {
            var newPoints = timeline_1.Timeline.points.slice(oldPoints);
            timeline_1.Timeline.vega.data("points").insert(newPoints);
            var first = timeline_1.Timeline.points[0]["time"];
            var last = timeline_1.Timeline.points[timeline_1.Timeline.points.length - 1]["time"];
            var duration = last == first ? 1 : last - first;
            var width = timeline_1.Timeline.vega.width();
            var dt = duration / width / 2; // fudge factor
            var mostRecent = -dt;
            timeline_1.Timeline.vega.data("points").remove(function (p) {
                if (p["time"] >= mostRecent + dt) {
                    mostRecent = p["time"];
                    return false;
                }
                return true;
            });
        }
        if (timeline_1.Timeline.highlightRegions.length - oldRegions > 0) {
            var newRegions = timeline_1.Timeline.highlightRegions.slice(oldRegions);
            timeline_1.Timeline.vega.data("highlight").insert(newRegions);
        }
        timeline_1.Timeline.vega.update();
        renderFlameGraph();
    }
    timeline_1.update = update;
    function computeTimelineData(events) {
        if (events.length == 0)
            return;
        // compute delta from first event
        if (timeline_1.Timeline.firstEvent === null)
            timeline_1.Timeline.firstEvent = events[0]["metrics"];
        var computePoint = function (p) {
            var ret = {};
            for (var _i = 0, _a = Object.keys(p); _i < _a.length; _i++) {
                var k = _a[_i];
                if (timeline_1.Timeline.firstEvent.hasOwnProperty(k))
                    ret[k] = p[k] - timeline_1.Timeline.firstEvent[k];
            }
            return ret;
        };
        // build up three things by walking the list of events:
        //  * a list of points on the graph (in Timeline.points)
        //  * a list of breakpoints for scrubbing (Timeline.breaks)
        //  * the callgraph (in Timeline.graph)
        var breaks = [];
        var points = [];
        if (timeline_1.Timeline.graph.root == null) {
            timeline_1.Timeline.graph.root = {
                "name": "root",
                "start": 0,
                "children": []
            };
            timeline_1.Timeline.graph.last = timeline_1.Timeline.graph.root;
        }
        var graph = timeline_1.Timeline.graph.last;
        for (var _i = 0; _i < events.length; _i++) {
            var e = events[_i];
            if (e["type"] == "ENTER") {
                var p = computePoint(e["metrics"]);
                var node = {
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
            }
            else if (e["type"] == "EXIT") {
                var p = computePoint(e["metrics"]);
                if (graph === timeline_1.Timeline.graph.root) {
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
            else if (e["type"] == "SAMPLE") {
                var p = computePoint(e["metrics"]);
                points.push(p);
                breaks.push([p["time"], graph, p]);
            }
        }
        // insert fake finish times into un-closed graph nodes
        var finish = events[events.length - 1]["metrics"]["time"] - timeline_1.Timeline.firstEvent["time"];
        var curr = graph;
        while (curr) {
            curr["finish"] = finish;
            curr["value"] = finish - curr["start"];
            curr = curr.parentPtr;
        }
        timeline_1.Timeline.graph.last = graph;
        for (var _a = 0; _a < points.length; _a++) {
            var p = points[_a];
            timeline_1.Timeline.points.push(p);
        }
        for (var _b = 0; _b < breaks.length; _b++) {
            var b = breaks[_b];
            timeline_1.Timeline.breaks.push(b);
        }
    }
    // ipts stands for infeasible pc times
    function computeHighlightRegions(ipts) {
        if (ipts.length == 0)
            return;
        var highlightRegions = [];
        for (var _i = 0; _i < ipts.length; _i++) {
            var ipt = ipts[_i];
            highlightRegions.push({
                "time": ipt.start - timeline_1.Timeline.firstEvent["time"],
                "value": 1,
                "type": "infeasible pc" // category of highlight
            });
            highlightRegions.push({
                "time": ipt.end - timeline_1.Timeline.firstEvent["time"],
                "value": 0,
                "type": "infeasible pc" // category of highlight
            });
        }
        for (var _a = 0; _a < highlightRegions.length; _a++) {
            var h = highlightRegions[_a];
            timeline_1.Timeline.highlightRegions.push(h);
        }
    }
    function renderTimeline() {
        var timeline = document.getElementById("timeline");
        var keys = {};
        for (var _i = 0, _a = timeline_1.Timeline.points; _i < _a.length; _i++) {
            var p = _a[_i];
            for (var k in p) {
                keys[k] = true;
            }
        }
        if (keys.hasOwnProperty("time"))
            delete keys["time"];
        var keyNames = Object.keys(keys);
        var scales = keyNames.map(function (k) {
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
        var spec = {
            "width": 800,
            "height": 400,
            "padding": "auto",
            "data": [{
                    "name": "points",
                    "values": timeline_1.Timeline.points,
                    "transform": [
                        { "type": "fold", "fields": keyNames },
                        { "type": "sort", "by": "-time" }
                    ]
                },
                {
                    "name": "highlight",
                    "values": timeline_1.Timeline.highlightRegions
                }],
            "scales": [{
                    "name": "x",
                    "type": "linear",
                    "domain": { "data": "points", "field": "time" },
                    "domainMin": { "signal": "xmin" },
                    "domainMax": { "signal": "xmax" },
                    "range": "width",
                    "round": true,
                    "zero": false
                },
                {
                    "name": "color",
                    "type": "ordinal",
                    "domain": { "data": "points", "field": "key" },
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
                }].concat(scales),
            "signals": [{
                    "name": "xtime",
                    "init": 0,
                    "streams": [{
                            "type": "mousemove",
                            "expr": "clamp(eventX(), 0, eventGroup('root').width)",
                            "scale": { "name": "x", "invert": true }
                        }]
                },
                { "name": "xmin" },
                { "name": "xmax" },
                { "name": "xminhover" },
                { "name": "xmaxhover" },
                { "name": "xhoveropacity", "init": 0 }
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
                        "labels": { "fontSize": { "value": 12 } },
                        "symbols": { "stroke": { "value": "transparent" } }
                    }
                },
                {
                    "title": "Highlights",
                    "fill": "highlight-color",
                    "orient": "right",
                    "offset": 0,
                    "properties": {
                        "labels": { "fontSize": { "value": 12 } },
                        "symbols": { "stroke": { "value": "transparent" }, "fillOpacity": { "value": 0.3 } }
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
                                    "y": { "scale": { "datum": "key" }, "field": "value" },
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
                            "x": { "scale": "x", "signal": "xminhover" },
                            "x2": { "scale": "x", "signal": "xmaxhover" },
                            "y": { "value": 0 },
                            "y2": { "field": { "group": "height" } },
                            "fill": { "value": "grey" },
                            "fillOpacity": { "signal": "xhoveropacity" }
                        }
                    }
                }]
        };
        vg.embed(timeline, { spec: spec }, function (err, res) {
            if (err) {
                console.error(err);
            }
            else {
                timeline_1.Timeline.vega = res.view;
                res.view.onSignal('xtime', timelineScrubCallback);
                // resize the graph
                windowResizeCallback();
            }
        });
        // initialize the callstack
        timelineScrubCallback("xtime", 0);
    }
    function timelineScrubCallback(signal, value) {
        var values = document.getElementById("stack-values");
        var functions = document.getElementById("stack-functions");
        // remove existing entries
        while (values.firstChild)
            values.removeChild(values.firstChild);
        while (functions.firstChild)
            functions.removeChild(functions.firstChild);
        // find the appropriate stack
        var i;
        for (i = 0; i < timeline_1.Timeline.breaks.length - 1; i++) {
            if (value < timeline_1.Timeline.breaks[i][0])
                break;
        }
        var _a = timeline_1.Timeline.breaks[i], _ = _a[0], graphNode = _a[1], metrics = _a[2];
        // render values
        var keys = Object.keys(metrics);
        keys.sort();
        for (var _i = 0; _i < keys.length; _i++) {
            var k = keys[_i];
            var node = document.createElement("li");
            var val = (metrics[k] % 1 == 0) ? metrics[k] : metrics[k].toFixed(2);
            node.innerHTML = "<b>" + k + "</b>: " + val;
            values.insertAdjacentElement("beforeend", node);
        }
        // figure out the stack
        var stack = [];
        while (graphNode) {
            var name_1 = graphNode["name"];
            graphNode = graphNode.parentPtr;
            if (name_1 == "#f")
                continue;
            if (stack.length > 0) {
                var last = stack[stack.length - 1];
                var lastName = (last instanceof Array ? last[0] : last);
                if (lastName == name_1) {
                    if (!(last instanceof Array))
                        stack[stack.length - 1] = [name_1, 1];
                    stack[stack.length - 1][1]++;
                }
                else {
                    stack.push(name_1);
                }
            }
            else {
                stack.push(name_1);
            }
        }
        // render the stack
        for (var _b = 0; _b < stack.length; _b++) {
            var entry = stack[_b];
            var node = document.createElement("li");
            var code = document.createElement("span");
            code.classList.add("code");
            var name_2 = (entry instanceof Array ? entry[0] : entry);
            code.innerHTML = name_2;
            node.insertAdjacentElement("beforeend", code);
            if (entry instanceof Array) {
                var count = document.createElement("span");
                count.classList.add("counter");
                count.innerHTML = entry[1];
                node.insertAdjacentElement("beforeend", count);
            }
            functions.insertAdjacentElement("beforeend", node);
        }
        // highlight flamegraph
        if (timeline_1.Timeline.flameGraph) {
            timeline_1.Timeline.flameGraph.highlight(value);
        }
    }
    function renderFlameGraph() {
        var rootStart = timeline_1.Timeline.graph.root["start"];
        var rootFinish = timeline_1.Timeline.graph.root["finish"];
        var width = timeline_1.Timeline.flameGraph ? timeline_1.Timeline.flameGraph.size()[0] : 1200;
        var minTime = (rootFinish - rootStart) / width;
        var rec = function (node) {
            var children = node["children"].filter(function (n) { return n["finish"] - n["start"] > minTime; }).map(rec);
            return {
                "name": node["name"],
                "start": node["start"],
                "finish": node["finish"],
                "value": node["value"],
                "children": children
            };
        };
        var root = rec(timeline_1.Timeline.graph.root);
        if (timeline_1.Timeline.flameGraph) {
            timeline_1.Timeline.flameGraph.data(root);
        }
        else {
            timeline_1.Timeline.flameGraph = d3.flameGraph("#flamegraph", root);
            timeline_1.Timeline.flameGraph.size([800, 300]);
            timeline_1.Timeline.flameGraph.zoomAction(flamegraphZoomCallback);
            timeline_1.Timeline.flameGraph.hoverAction(flamegraphHoverCallback);
        }
        timeline_1.Timeline.flameGraph.render();
    }
    function windowResizeCallback() {
        if (timeline_1.Timeline.vega && !timeline_1.Timeline.resizing) {
            timeline_1.Timeline.resizing = true;
            window.setTimeout(function () {
                var stackWidth = document.getElementById("stack").clientWidth;
                var windowWidth = window.innerWidth;
                var padding = timeline_1.Timeline.vega.padding();
                var pad = padding.left + padding.right;
                var newWidth = windowWidth - stackWidth - pad - 80;
                timeline_1.Timeline.vega.width(newWidth).update();
                if (timeline_1.Timeline.flameGraph) {
                    timeline_1.Timeline.flameGraph.size([newWidth, 300]).render();
                    document.getElementById("flamegraph").style.marginLeft = padding.left;
                }
                timeline_1.Timeline.resizing = false;
            }, 50);
        }
    }
    function flamegraphZoomCallback(node) {
        timeline_1.Timeline.vega.signal("xmin", node["start"]);
        timeline_1.Timeline.vega.signal("xmax", node["finish"]);
        timeline_1.Timeline.vega.update();
    }
    function flamegraphHoverCallback(node, enter) {
        if (!timeline_1.Timeline.vega)
            return;
        var start = node["start"];
        var finish = node["finish"];
        if (enter) {
            timeline_1.Timeline.vega.signal("xminhover", node["start"]);
            timeline_1.Timeline.vega.signal("xmaxhover", node["finish"]);
            timeline_1.Timeline.vega.signal("xhoveropacity", 0.2);
            timeline_1.Timeline.vega.signal("xtime", node["start"] + 1e-10);
        }
        else {
            timeline_1.Timeline.vega.signal("xhoveropacity", 0.0);
        }
        timeline_1.Timeline.vega.update();
    }
})(timeline || (timeline = {})); // /namespace
document.addEventListener("DOMContentLoaded", dataOnload(timeline.init, timeline.update));
