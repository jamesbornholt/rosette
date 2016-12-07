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

    function initTimelineData(metric: string) {
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
        let dt = root["start"]["time"];
        let stack = [];
        let rec = (node) => {
            let start = node["start"];
            let t1 = start["time"] - dt;
            // push start data point
            Timeline.points.push({ "time": t1, "value": start[metric] });
            if (node["function"] != "#f")
                stack.push(getFunctionName(node["function"]));
            let children = node["children"].slice() as Array<any>;
            children.sort((a, b) => a["start"]["time"] - b["start"]["time"]);
            for (let c of children) {
                // before entry, current stack
                let t_entry = c["start"]["time"] - dt;
                Timeline.breaks.push(t_entry);
                Timeline.stacks.push(stack.slice());
                // recurse on c, will push a pre-exit stack
                rec(c);
            }
            let finish = node["finish"];
            let t2 = finish["time"] - dt;
            // push pre-exit stack
            Timeline.breaks.push(t2);
            Timeline.stacks.push(stack.slice());
            // remove self from stack
            stack.pop();
        };
        rec(root);
    }

    function renderTimeline(metric: string) {
        let timeline = document.getElementById("timeline");

        let points = [];
        let startTime = Infinity;
        for (let func of Data.functions) {
            for (let fcall of func["calls"]) {
                for (let key of ["start", "finish"]) {
                    if (!fcall.hasOwnProperty(key))
                        continue;
                    let data = fcall[key];
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
        let spec = {
            "width": 800,
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
        }

        vg.embed(timeline, { spec: spec }, function (err, res) {
            res.view.onSignal('xtime', timelineScrubCallback);
        });

        // initialize the callstack
        timelineScrubCallback("xtime", 0);
    }

    function timelineScrubCallback(signal: string, value: number) {
        // update timestamp
        let ts = document.querySelector("#stack-time");
        ts.innerHTML = (value / 1000).toFixed(2);
        let ul = document.querySelector("#stack ul");
        // remove existing entries
        while (ul.firstChild)
            ul.removeChild(ul.firstChild);
        // find the appropriate stack
        var i = 0;
        for (i = 0; i < Timeline.breaks.length; i++) {
            if (value <= Timeline.breaks[i])
                break;
        }
        let stack = Timeline.stacks[i];
        // render new stack
        let lastEntry = null;
        let lastNode = null as HTMLLIElement;
        for (let entry of stack) {
            if (entry == lastEntry) {
                var counter = lastNode.querySelector(".counter");
                if (!counter) {
                    counter = document.createElement("span");
                    counter.innerHTML = "0";
                    counter.classList.add("counter");
                    lastNode.insertAdjacentElement("beforeend", counter);
                }
                counter.innerHTML = (parseInt(counter.innerHTML) + 1).toString();
            } else {
                let node = document.createElement("li");
                let code = document.createElement("span");
                code.innerHTML = entry;
                code.classList.add("code");
                node.insertAdjacentElement("beforeend", code);
                ul.insertAdjacentElement("beforeend", node);
                lastNode = node;
                lastEntry = entry;
            }
        }
    }

} // /namespace

document.addEventListener("DOMContentLoaded", timeline.init);