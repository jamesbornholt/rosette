/// <reference path="data.ts" />

// convince TS that document.querySelectorAll can be an array
interface NodeList extends Array<Node> { }
interface HTMLCollection extends Array<Element> { }

namespace timeline {

    declare var vg; // vega
    declare var vl; // vega-lite

    export let Timeline = {
        breaks: [],
        stacks: [],
        points: []
    }

    export function init() {
        // Initialize the profile data
        initData();
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
    }

    function initTimelineData() {
        // walk the profile graph
        let root = Data.graph;
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
        let rec = (node) => {
            let start = computePoint(node["start"]);
            // push start data point
            stack.push(node);
            let children = node["children"].slice() as Array<any>;
            children.sort((a, b) => a["start"]["time"] - b["start"]["time"]);
            for (let c of children) {
                // before entry, current stack
                let p = computePoint(c["start"]);
                Timeline.breaks.push(p["time"]);
                Timeline.stacks.push(stack.slice());
                Timeline.points.push(p);
                // recurse on c, will push a pre-exit stack
                rec(c);
            }
            let finish = computePoint(node["finish"]);
            // push pre-exit stack
            Timeline.breaks.push(finish["time"]);
            Timeline.stacks.push(stack.slice());
            Timeline.points.push(finish);
            // remove self from stack
            stack.pop();
        };
        rec(root);
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
        }

        vg.embed(timeline, { spec: spec }, function (err, res) {
            if (err)
                console.error(err);
            else
                res.view.onSignal('xtime', timelineScrubCallback);
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
        let point = Timeline.points[i];

        // render values
        let keys = Object.keys(point);
        keys.sort();
        for (let k of keys) {
            let node = document.createElement("li");
            node.innerHTML = "<b>" + k + "</b>: " + point[k];
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

} // /namespace

document.addEventListener("DOMContentLoaded", timeline.init);