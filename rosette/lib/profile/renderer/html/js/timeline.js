/// <reference path="data.ts" />
var timeline;
(function (timeline_1) {
    timeline_1.Timeline = {
        breaks: [],
        stacks: [],
        points: []
    };
    function init() {
        // Initialize the profile data
        initData();
        // Cache the stacks at each timestep
        initTimelineData("terms");
        // update the profile source info
        document.getElementById("name").innerHTML = "Timeline: " + Data.metadata.name;
        document.getElementById("source").innerHTML = Data.metadata.source;
        document.getElementById("form").innerHTML = Data.metadata.form;
        document.getElementById("time").innerHTML = Data.metadata.time;
        document.title = "Timeline: " + Data.metadata.name;
        // Render the timeline
        renderTimeline("terms");
    }
    timeline_1.init = init;
    function initTimelineData(metric) {
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
        var dt = root["start"]["time"];
        var stack = [];
        var rec = function (node) {
            var start = node["start"];
            var t1 = start["time"] - dt;
            // push start data point
            timeline_1.Timeline.points.push({ "time": t1, "value": start[metric] });
            stack.push(node["function"]);
            var children = node["children"].slice();
            children.sort(function (a, b) { return a["start"]["time"] - b["start"]["time"]; });
            for (var _i = 0, children_1 = children; _i < children_1.length; _i++) {
                var c = children_1[_i];
                // before entry, current stack
                var t_entry = c["start"]["time"] - dt;
                timeline_1.Timeline.breaks.push(t_entry);
                timeline_1.Timeline.stacks.push(stack.slice());
                // recurse on c, will push a pre-exit stack
                rec(c);
            }
            var finish = node["finish"];
            var t2 = finish["time"] - dt;
            // push pre-exit stack
            timeline_1.Timeline.breaks.push(t2);
            timeline_1.Timeline.stacks.push(stack.slice());
            // remove self from stack
            stack.pop();
        };
        rec(root);
    }
    function renderTimeline(metric) {
        var timeline = document.getElementById("timeline");
        var points = [];
        var startTime = Infinity;
        for (var _i = 0, _a = Data.functions; _i < _a.length; _i++) {
            var func = _a[_i];
            for (var _b = 0, _c = func["calls"]; _b < _c.length; _b++) {
                var fcall = _c[_b];
                for (var _d = 0, _e = ["start", "finish"]; _d < _e.length; _d++) {
                    var key = _e[_d];
                    if (!fcall.hasOwnProperty(key))
                        continue;
                    var data = fcall[key];
                    if (!data.hasOwnProperty("time") || !data.hasOwnProperty(metric))
                        continue;
                    points.push({ "time": data["time"], "value": data[metric] });
                    if (data["time"] < startTime)
                        startTime = data["time"];
                }
            }
        }
        for (var i = 0; i < points.length; i++) {
            points[i]["time"] = points[i]["time"] - startTime;
        }
        // render the vega spec
        var spec = {
            "width": 400,
            "height": 400,
            "padding": "auto",
            "data": [{ "name": "points", "values": points }],
            "scales": [{
                    "name": "x",
                    "type": "linear",
                    "domain": { "data": "points", "field": "time" },
                    "range": "width",
                    "round": true
                },
                {
                    "name": "y",
                    "type": "linear",
                    "domain": { "data": "points", "field": "value" },
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
                    "scale": "y",
                    "format": "s",
                    "grid": true,
                    "layer": "back",
                    "title": "terms"
                }],
            "marks": [{
                    "name": "root",
                    "type": "line",
                    "from": { "data": "points", "transform": [{ "type": "sort", "by": "-time" }] },
                    "properties": {
                        "update": {
                            "x": { "scale": "x", "field": "time" },
                            "y": { "scale": "y", "field": "value" },
                            "stroke": { "value": "#4682b4" },
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
            res.view.onSignal('xtime', timelineScrubCallback);
        });
    }
    function timelineScrubCallback(signal, value) {
        var ul = document.querySelector("#stack ul");
        // remove existing entries
        while (ul.firstChild)
            ul.removeChild(ul.firstChild);
        // find the appropriate stack
        var i = 0;
        for (i = 0; i < timeline_1.Timeline.breaks.length; i++) {
            if (value <= timeline_1.Timeline.breaks[i])
                break;
        }
        var stack = timeline_1.Timeline.stacks[i];
        // render new stack
        var lastEntry = null;
        var lastNode = null;
        for (var _i = 0, stack_1 = stack; _i < stack_1.length; _i++) {
            var entry = stack_1[_i];
            if (entry == lastEntry) {
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
                var node = document.createElement("li");
                var code = document.createElement("span");
                code.innerHTML = entry;
                code.classList.add("code");
                node.insertAdjacentElement("beforeend", code);
                ul.insertAdjacentElement("beforeend", node);
                lastNode = node;
                lastEntry = entry;
            }
        }
    }
})(timeline || (timeline = {})); // /namespace
document.addEventListener("DOMContentLoaded", timeline.init);
//# sourceMappingURL=timeline.js.map