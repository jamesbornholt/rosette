/// <reference path="data.ts" />
var timeline;
(function (timeline_1) {
    timeline_1.Timeline = {
        breaks: [],
        stacks: [],
        points: [],
        vega: null,
        resizing: false
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
        // Render the timeline
        renderTimeline();
        // Bind the resize handler
        // window.addEventListener("resize", windowResizeCallback);
    }
    timeline_1.init = init;
    function update() {
        var oldLength = timeline_1.Timeline.points.length;
        initTimelineData();
        console.log("update(): " + oldLength + " -> " + timeline_1.Timeline.points.length);
        console.log("samples: ", Data.samples.length);
        if (timeline_1.Timeline.points.length > oldLength) {
            var newPoints = timeline_1.Timeline.points.slice(oldLength);
            timeline_1.Timeline.vega.data("points").insert(newPoints);
            timeline_1.Timeline.vega.update();
        }
    }
    timeline_1.update = update;
    function initTimelineData() {
        // walk the profile graph
        var root = Data.graph;
        // let root =
        // {
        //     "start": {"time": 0, "terms": 0},
        //     "finish": {"time": 100, "terms": 0},
        //     "function": "a",
        //     "children": [
        //         {
        //             "start": {"time": 1, "terms": 0},
        //             "finish": {"time": 20, "terms": 0},
        //             "function": "b",
        //             "children": [
        //                 {
        //                     "start": {"time": 4, "terms": 0},
        //                     "finish": {"time": 10, "terms": 0},
        //                     "function": "c",
        //                     "children": []
        //                 },
        //                 {
        //                     "start": {"time": 14, "terms": 0},
        //                     "finish": {"time": 19, "terms": 0},
        //                     "function": "d",
        //                     "children": []
        //                 }
        //             ]
        //         },
        //         {
        //             "start": {"time": 23, "terms": 0},
        //             "finish": {"time": 94, "terms": 0},
        //             "function": "e",
        //             "children": []
        //         }
        //     ]
        // };
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
        var stack = [];
        var breaks = [], stacks = [], points = [];
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
        console.log(points.length);
        points = points.concat(Data.samples.map(computePoint));
        points.sort(function (a, b) { return a["time"] - b["time"]; });
        console.log(points.length);
        timeline_1.Timeline.points = points;
        timeline_1.Timeline.stacks = stacks;
        timeline_1.Timeline.breaks = breaks; // done last for race condition
    }
    function renderTimeline() {
        var timeline = document.getElementById("timeline");
        // render the vega spec
        var spec = {
            "width": 800,
            "height": 400,
            "padding": "auto",
            "data": [{ "name": "points", "values": timeline_1.Timeline.points }],
            "scales": [{
                    "name": "x",
                    "type": "linear",
                    "domain": { "data": "points", "field": "time" },
                    "range": "width",
                    "round": true
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
                }],
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
    }
    function windowResizeCallback() {
        if (timeline_1.Timeline.vega && !timeline_1.Timeline.resizing) {
            timeline_1.Timeline.resizing = true;
            window.setTimeout(function () {
                var panel = document.getElementById("timeline-panel");
                var width = panel.clientWidth;
                console.log("resizing", width);
                timeline_1.Timeline.vega.width(width - 150).update();
                timeline_1.Timeline.resizing = false;
            }, 50);
        }
    }
})(timeline || (timeline = {})); // /namespace
document.addEventListener("DOMContentLoaded", dataOnload(timeline.init, timeline.update));
//# sourceMappingURL=timeline.js.map