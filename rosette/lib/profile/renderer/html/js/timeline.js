/// <reference path="data.ts" />
var timeline;
(function (timeline_1) {
    timeline_1.Timeline = {
        breaks: [],
        stacks: [],
        points: [],
        vega: null,
        resizing: false,
        flameGraph: null
    };
    function init() {
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
        window.addEventListener("resize", windowResizeCallback);
    }
    timeline_1.init = init;
    function update() {
        initTimelineData();
        console.log(timeline_1.Timeline.points.length);
        timeline_1.Timeline.vega.data("points").remove(function (d) { return true; });
        timeline_1.Timeline.vega.data("points").insert(timeline_1.Timeline.points);
        timeline_1.Timeline.vega.update();
    }
    timeline_1.update = update;
    function initTimelineData() {
        // walk the profile graph
        var root = Data.graph;
        var first = root["start"];
        // compute difference between a point and the first point
        var computePoint = function (p) {
            var ret = {};
            for (var _i = 0, _a = Object.keys(p); _i < _a.length; _i++) {
                var k = _a[_i];
                if (first.hasOwnProperty(k))
                    ret[k] = p[k] - first[k];
            }
            return ret;
        };
        // push starting point
        var init = computePoint(first);
        var stack = [];
        var breaks = [init["time"]], stacks = [[root]], points = [init];
        var rec = function (node) {
            var start = computePoint(node["start"]);
            // push start data point
            stack.push(node);
            var children = node["children"].slice();
            children.sort(function (a, b) { return a["start"]["time"] - b["start"]["time"]; });
            for (var _i = 0, children_1 = children; _i < children_1.length; _i++) {
                var c = children_1[_i];
                // before entry, current stack
                var p = computePoint(c["start"]);
                breaks.push(p["time"]);
                stacks.push(stack.slice());
                points.push(p);
                // recurse on c, will push a pre-exit stack
                rec(c);
            }
            var finish = computePoint(node["finish"]);
            // push pre-exit stack
            breaks.push(finish["time"]);
            stacks.push(stack.slice());
            points.push(finish);
            // remove self from stack
            stack.pop();
        };
        rec(root);
        points = points.concat(Data.samples.map(computePoint));
        points.sort(function (a, b) { return a["time"] - b["time"]; });
        timeline_1.Timeline.points = points;
        timeline_1.Timeline.stacks = stacks;
        timeline_1.Timeline.breaks = breaks; // done last for race condition
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
                    "fill": "color",
                    "orient": "left",
                    "offset": -150,
                    "properties": {
                        "labels": { "fontSize": { "value": 12 } },
                        "symbols": { "stroke": { "value": "transparent" } }
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
        var i = 0;
        for (i = 0; i < timeline_1.Timeline.breaks.length; i++) {
            if (value <= timeline_1.Timeline.breaks[i])
                break;
        }
        var stack = timeline_1.Timeline.stacks[i];
        var j = 0;
        for (j = 0; j < timeline_1.Timeline.points.length; j++) {
            if (value <= timeline_1.Timeline.points[j]["time"])
                break;
        }
        var point = timeline_1.Timeline.points[j];
        // render values
        var keys = Object.keys(point);
        keys.sort();
        for (var _i = 0, keys_1 = keys; _i < keys_1.length; _i++) {
            var k = keys_1[_i];
            var node = document.createElement("li");
            var val = (point[k] % 1 == 0) ? point[k] : point[k].toFixed(2);
            node.innerHTML = "<b>" + k + "</b>: " + val;
            values.insertAdjacentElement("beforeend", node);
        }
        // render new stack
        var lastEntry = null;
        var lastNode = null;
        for (var _a = 0, stack_1 = stack; _a < stack_1.length; _a++) {
            var entry = stack_1[_a];
            if (lastEntry && entry["function"] == lastEntry["function"]) {
                var counter = lastNode.querySelector(".counter");
                if (!counter) {
                    counter = document.createElement("span");
                    counter.innerHTML = "0";
                    counter.classList.add("counter");
                    lastNode.insertAdjacentElement("beforeend", counter);
                }
                counter.innerHTML = (parseInt(counter.innerHTML) + 1).toString();
            }
            else {
                if (entry["function"] == "#f")
                    continue;
                var node = document.createElement("li");
                var code = document.createElement("span");
                code.innerHTML = getFunctionName(entry["function"]);
                code.classList.add("code");
                node.insertAdjacentElement("beforeend", code);
                functions.insertAdjacentElement("beforeend", node);
                lastNode = node;
                lastEntry = entry;
            }
        }
        // highlight flamegraph
        if (timeline_1.Timeline.flameGraph) {
            timeline_1.Timeline.flameGraph.highlight(value);
        }
    }
    function renderFlameGraph() {
        var dt = Data.graph["start"]["time"];
        var rec = function (node) {
            var children = node["children"].map(rec);
            children.sort(function (a, b) { return a["start"] - b["start"]; });
            return {
                "name": node["function"],
                "value": node["finish"]["time"] - node["start"]["time"],
                "start": node["start"]["time"] - dt,
                "finish": node["finish"]["time"] - dt,
                "children": children
            };
        };
        var graph = rec(Data.graph);
        timeline_1.Timeline.flameGraph = d3.flameGraph("#flamegraph", graph);
        timeline_1.Timeline.flameGraph.zoomAction(flamegraphZoomCallback);
        timeline_1.Timeline.flameGraph.hoverAction(flamegraphHoverCallback);
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
                timeline_1.Timeline.flameGraph.size([newWidth, 400]).render();
                document.getElementById("flamegraph").style.marginLeft = padding.left;
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
//# sourceMappingURL=timeline.js.map